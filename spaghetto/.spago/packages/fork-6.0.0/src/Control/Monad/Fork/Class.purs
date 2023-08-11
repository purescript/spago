module Control.Monad.Fork.Class where

import Prelude hiding (join)

import Effect.Aff as Aff
import Control.Monad.Error.Class (class MonadThrow, class MonadError)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Control.Monad.Trans.Class (lift)

-- | Represents Monads which can be forked asynchronously.
-- |
-- | Laws:
-- |
-- | ```purescript
-- | -- Unjoined suspension is a no-op
-- | suspend a1 *> suspend a2 = suspend a2
-- |
-- | -- Suspend/join is identity
-- | suspend >=> join = id
-- |
-- | -- Fork/join is identity
-- | fork >=> join = id
-- |
-- | -- Join is idempotent
-- | join t *> join t = join t
-- | ```
class (Monad m, Functor f) <= MonadFork f m | m -> f where
  suspend :: forall a. m a -> m (f a)
  fork :: forall a. m a -> m (f a)
  join :: forall a. f a -> m a

instance monadForkAff :: MonadFork Aff.Fiber Aff.Aff where
  suspend = Aff.suspendAff
  fork = Aff.forkAff
  join = Aff.joinFiber

instance monadForkReaderT :: MonadFork f m => MonadFork f (ReaderT r m) where
  suspend (ReaderT ma) = ReaderT (suspend <<< ma)
  fork (ReaderT ma) = ReaderT (fork <<< ma)
  join = lift <<< join

-- | Represents Monads which can be killed after being forked.
-- |
-- | Laws:
-- |
-- | ```purescript
-- | -- Killed suspension is an exception
-- | suspend a >>= \f -> kill e f *> join f = throwError e
-- |
-- | -- Suspend/kill is unit
-- | suspend a >>= kill e = pure unit
-- | ```
class (MonadFork f m, MonadThrow e m) <= MonadKill e f m | m -> e f where
  kill :: forall a. e -> f a -> m Unit

instance monadKillAff :: MonadKill Aff.Error Aff.Fiber Aff.Aff where
  kill = Aff.killFiber

instance monadKillReaderT :: MonadKill e f m => MonadKill e f (ReaderT r m) where
  kill e = lift <<< kill e

data BracketCondition e a
  = Completed a
  | Failed e
  | Killed e

-- | Represents Monads which support cleanup in the presence of async
-- | exceptions.
-- |
-- | Laws:
-- | ```purescript
-- | bracket a k \_ -> pure r
-- |   = uninterruptible (a >>= k (Completed r))
-- |
-- | -- Release failed
-- | bracket a k \_ -> throwError e
-- |   = uninterruptible (a >>= k (Failed e) *> throwError e)
-- |
-- | -- Release killed
-- | fork (bracket a k \_ -> never) >>= \f -> kill e f *> void (try (join f))
-- |   = uninterruptible (a >>= k (Killed e))
-- | ```
class (MonadKill e f m, MonadError e m) <= MonadBracket e f m | m -> e f where
  bracket :: forall r a. m r -> (BracketCondition e a -> r -> m Unit) -> (r -> m a) -> m a
  uninterruptible :: forall a. m a -> m a
  never :: forall a. m a

instance monadBracketAff :: MonadBracket Aff.Error Aff.Fiber Aff.Aff where
  bracket acquire release run =
    Aff.generalBracket acquire
      { completed: release <<< Completed
      , failed: release <<< Failed
      , killed: release <<< Killed
      }
      run
  uninterruptible = Aff.invincible
  never = Aff.never

instance monadBracketReaderT :: MonadBracket e f m => MonadBracket e f (ReaderT r m) where
  bracket (ReaderT acquire) release run = ReaderT \r ->
    bracket (acquire r)
      (\c a -> runReaderT (release c a) r)
      (\a -> runReaderT (run a) r)
  uninterruptible k = ReaderT \r ->
    uninterruptible (runReaderT k r)
  never = lift never
