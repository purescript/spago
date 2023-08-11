-- | This module defines the reader-writer-state monad transformer, `RWST`.

module Control.Monad.RWS.Trans
  ( RWSResult(..)
  , RWST(..), runRWST, evalRWST, execRWST, mapRWST, withRWST
  , module Control.Monad.Trans.Class
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError, catchError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadWriter, class MonadTell)
import Control.Plus (class Plus, empty)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Class (class MonadEffect, liftEffect)

data RWSResult state result writer = RWSResult state result writer

-- | The reader-writer-state monad transformer, which combines the operations
-- | of `ReaderT`, `WriterT` and `StateT` into a single monad transformer.
newtype RWST r w s m a = RWST (r -> s -> m (RWSResult s a w))

-- | Run a computation in the `RWST` monad.
runRWST :: forall r w s m a. RWST r w s m a -> r -> s -> m (RWSResult s a w)
runRWST (RWST x) = x

-- | Run a computation in the `RWST` monad, discarding the final state.
evalRWST :: forall r w s m a. Monad m => RWST r w s m a -> r -> s -> m (Tuple a w)
evalRWST (RWST m) r s = m r s >>= \(RWSResult _ result writer) -> pure (Tuple result writer)

-- | Run a computation in the `RWST` monad, discarding the result.
execRWST :: forall r w s m a. Monad m => RWST r w s m a -> r -> s -> m (Tuple s w)
execRWST (RWST m) r s = m r s >>= \(RWSResult state _ writer) -> pure (Tuple state writer)

-- | Change the result and accumulator types in a `RWST` monad action.
mapRWST :: forall r w1 w2 s m1 m2 a1 a2. (m1 (RWSResult s a1 w1) -> m2 (RWSResult s a2 w2)) -> RWST r w1 s m1 a1 -> RWST r w2 s m2 a2
mapRWST f (RWST m) = RWST \r s -> f (m r s)

-- | Change the context type in a `RWST` monad action.
withRWST :: forall r1 r2 w s m a. (r2 -> s -> Tuple r1 s) -> RWST r1 w s m a -> RWST r2 w s m a
withRWST f m = RWST \r s -> uncurry (case m of RWST m' -> m') (f r s)

derive instance newtypeRWST :: Newtype (RWST r w s m a) _

instance functorRWST :: (Functor m) => Functor (RWST r w s m) where
  map f (RWST m) = RWST \r s ->
    (\(RWSResult state result writer) -> RWSResult state (f result) writer) <$> m r s

instance applyRWST :: (Bind m, Monoid w) => Apply (RWST r w s m) where
  apply (RWST f) (RWST m) = RWST \r s ->
    f r s >>= \(RWSResult s' f' w') ->
    m r s' <#> \(RWSResult s'' a'' w'') ->
    RWSResult s'' (f' a'') (w' <> w'')

instance altRWST :: Alt m => Alt (RWST r w s m) where
  alt (RWST m) (RWST n) = RWST $ \ r s -> m r s <|> n r s

instance alternativeRWST :: (Monoid w, Alternative m, Monad m) => Alternative (RWST r w s m)

instance bindRWST :: (Bind m, Monoid w) => Bind (RWST r w s m) where
  bind (RWST m) f = RWST \r s ->
    m r s >>= \(RWSResult s' a w) ->
      case (f a) of
        RWST f' -> f' r s' <#> \(RWSResult state result writer) ->
          RWSResult state result (w <> writer)

instance applicativeRWST :: (Monad m, Monoid w) => Applicative (RWST r w s m) where
  pure a = RWST \_ s -> pure $ RWSResult s a mempty

instance monadRWST :: (Monad m, Monoid w) => Monad (RWST r w s m)

instance monadTransRWST :: Monoid w => MonadTrans (RWST r w s) where
  lift m = RWST \_ s -> m >>= \a -> pure $ RWSResult s a mempty

instance lazyRWST :: Lazy (RWST r w s m a) where
  defer f = RWST \r s -> case f unit of RWST f' -> f' r s

instance monadEffectRWS :: (Monoid w, MonadEffect m) => MonadEffect (RWST r w s m) where
  liftEffect = lift <<< liftEffect

instance monadAskRWST :: (Monad m, Monoid w) => MonadAsk r (RWST r w s m) where
  ask = RWST \r s -> pure $ RWSResult s r mempty

instance monadReaderRWST :: (Monad m, Monoid w) => MonadReader r (RWST r w s m) where
  local f m = RWST \r s -> case m of RWST m' -> m' (f r) s

instance monadStateRWST :: (Monad m, Monoid w) => MonadState s (RWST r w s m) where
  state f = RWST \_ s -> case f s of Tuple a s' -> pure $ RWSResult s' a mempty

instance monadTellRWST :: (Monad m, Monoid w) => MonadTell w (RWST r w s m) where
  tell w = RWST \_ s -> pure $ RWSResult s unit w

instance monadWriterRWST :: (Monad m, Monoid w) => MonadWriter w (RWST r w s m) where
  listen m = RWST \r s ->
    case m of
      RWST m' -> m' r s >>= \(RWSResult s' a w) ->
        pure $ RWSResult s' (Tuple a w) w
  pass m = RWST \r s ->
    case m of
      RWST m' -> m' r s >>= \(RWSResult s' (Tuple a f) w) ->
        pure $ RWSResult s' a (f w)

instance monadThrowRWST :: (MonadThrow e m, Monoid w) => MonadThrow e (RWST r w s m) where
  throwError e = lift (throwError e)

instance monadErrorRWST :: (MonadError e m, Monoid w) => MonadError e (RWST r w s m) where
  catchError m h = RWST $ \r s ->
    catchError
      (case m of RWST m' -> m' r s)
      (\e -> case h e of RWST m' -> m' r s)

instance monadRecRWST :: (MonadRec m, Monoid w) => MonadRec (RWST r w s m) where
  tailRecM k a = RWST \r s -> tailRecM (k' r) (RWSResult s a mempty)
    where
    k' r (RWSResult state result writer) =
      case k result of
        RWST m -> do
          RWSResult state' result' writer' <- m r state
          pure case result' of
            Loop x -> Loop (RWSResult state' x (writer <> writer'))
            Done y -> Done (RWSResult state' y (writer <> writer'))

instance plusRWST :: Plus m => Plus (RWST r w s m) where
  empty = RWST \ _ _ -> empty

instance semigroupRWST :: (Bind m, Monoid w, Semigroup a) => Semigroup (RWST r w s m a) where
  append = lift2 (<>)

instance monoidRWST :: (Monad m, Monoid w, Monoid a) => Monoid (RWST r w s m a) where
  mempty = pure mempty

