-- | This module defines the CPS monad transformer.

module Control.Monad.Cont.Trans
  ( ContT(..), runContT, mapContT, withContT
  , module Control.Monad.Trans.Class
  , module Control.Monad.Cont.Class
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect, liftEffect)

-- | The CPS monad transformer.
-- |
-- | This monad transformer extends the base monad with the operation `callCC`.
newtype ContT :: forall k. k -> (k -> Type) -> Type -> Type
newtype ContT r m a = ContT ((a -> m r) -> m r)

-- | Run a computation in the `ContT` monad, by providing a continuation.
runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
runContT (ContT f) k = f k

-- | Modify the underlying action in a `ContT` monad action.
mapContT :: forall r m a. (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f (ContT m) = ContT (\k -> f (m k))

-- | Modify the continuation in a `ContT` monad action
withContT :: forall r m a b. ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f (ContT m) = ContT (\k -> m (f k))

derive instance newtypeContT :: Newtype (ContT r m a) _

instance monadContContT :: Monad m => MonadCont (ContT r m) where
  callCC f = ContT (\k -> case f (\a -> ContT (\_ -> k a)) of ContT f' -> f' k)

instance functorContT :: Functor m => Functor (ContT r m) where
  map f (ContT m) = ContT (\k -> m (\a -> k $ f a))

instance applyContT :: Apply m => Apply (ContT r m) where
  apply (ContT f) (ContT v) = ContT (\k -> f (\g -> v (\a -> k (g a))))

instance applicativeContT :: Applicative m => Applicative (ContT r m) where
  pure a = ContT (\k -> k a)

instance bindContT :: Bind m => Bind (ContT r m) where
  bind (ContT m) k = ContT (\k' -> m (\a -> case k a of ContT m' -> m' k'))

instance monadContT :: Monad m => Monad (ContT r m)

instance monadTransContT :: MonadTrans (ContT r) where
  lift m = ContT (\k -> m >>= k)

instance monadEffectContT :: MonadEffect m => MonadEffect (ContT r m) where
  liftEffect = lift <<< liftEffect

instance monadAskContT :: MonadAsk r1 m => MonadAsk r1 (ContT r m) where
  ask = lift ask

instance monadReaderContT :: MonadReader r1 m => MonadReader r1 (ContT r m) where
  local f (ContT c) = ContT \k -> do
    r <- ask
    local f (c (local (const (r :: r1)) <<< k))

instance monadStateContT :: MonadState s m => MonadState s (ContT r m) where
  state = lift <<< state

instance semigroupContT :: (Apply m, Semigroup a) => Semigroup (ContT r m a) where
  append = lift2 (<>)

instance monoidContT :: (Applicative m, Monoid a) => Monoid (ContT r m a) where
  mempty = pure mempty
