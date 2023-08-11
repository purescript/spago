-- | This module defines the reader monad transformer, `ReaderT`.

module Control.Monad.Reader.Trans
  ( ReaderT(..), runReaderT, withReaderT, mapReaderT
  , module Control.Monad.Trans.Class
  , module Control.Monad.Reader.Class
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, catchError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, asks, local)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadWriter, class MonadTell, pass, listen, tell)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus, empty)
import Data.Distributive (class Distributive, distribute, collect)
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect, liftEffect)

-- | The reader monad transformer.
-- |
-- | This monad transformer extends the base monad transformer with a _global context_ of
-- | type `r`.
-- |
-- | The `MonadReader` type class describes the operations supported by this monad.
newtype ReaderT :: forall k. Type -> (k -> Type) -> k -> Type
newtype ReaderT r m a = ReaderT (r -> m a)

-- | Run a computation in the `ReaderT` monad.
runReaderT :: forall r m a. ReaderT r m a -> (r -> m a)
runReaderT (ReaderT x) = x

-- | Change the type of the result in a `ReaderT` monad action.
mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b
mapReaderT f (ReaderT m) = ReaderT (f <<< m)

-- | Change the type of the context in a `ReaderT` monad action.
withReaderT :: forall r1 r2 m a. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
withReaderT f (ReaderT m) = ReaderT (m <<< f)

derive instance newtypeReaderT :: Newtype (ReaderT r m a) _

instance functorReaderT :: Functor m => Functor (ReaderT r m) where
  map = mapReaderT <<< map

instance applyReaderT :: Apply m => Apply (ReaderT r m) where
  apply (ReaderT f) (ReaderT v) = ReaderT \r -> f r <*> v r

instance applicativeReaderT :: Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT <<< const <<< pure

instance altReaderT :: Alt m => Alt (ReaderT r m) where
  alt (ReaderT m) (ReaderT n) = ReaderT \r -> m r <|> n r

instance plusReaderT :: Plus m => Plus (ReaderT r m) where
  empty = ReaderT (const empty)

instance alternativeReaderT :: Alternative m => Alternative (ReaderT r m)

instance bindReaderT :: Bind m => Bind (ReaderT r m) where
  bind (ReaderT m) k = ReaderT \r ->
    m r >>= \a -> case k a of ReaderT f -> f r

instance monadReaderT :: Monad m => Monad (ReaderT r m)

instance semigroupReaderT :: (Apply m, Semigroup a) => Semigroup (ReaderT s m a) where
  append = lift2 (<>)

instance monoidReaderT :: (Applicative m, Monoid a) => Monoid (ReaderT s m a) where
  mempty = pure mempty

instance monadPlusReaderT :: MonadPlus m => MonadPlus (ReaderT r m)

instance monadTransReaderT :: MonadTrans (ReaderT r) where
  lift = ReaderT <<< const

instance monadEffectReader :: MonadEffect m => MonadEffect (ReaderT r m) where
  liftEffect = lift <<< liftEffect

instance monadContReaderT :: MonadCont m => MonadCont (ReaderT r m) where
  callCC f = ReaderT \r -> callCC \c ->
    case f (ReaderT <<< const <<< c) of ReaderT f' -> f' r

instance monadThrowReaderT :: MonadThrow e m => MonadThrow e (ReaderT r m) where
  throwError = lift <<< throwError

instance monadErrorReaderT :: MonadError e m => MonadError e (ReaderT r m) where
  catchError (ReaderT m) h =
    ReaderT \r -> catchError (m r) (\e -> case h e of ReaderT f -> f r)

instance monadAskReaderT :: Monad m => MonadAsk r (ReaderT r m) where
  ask = ReaderT pure

instance monadReaderReaderT :: Monad m => MonadReader r (ReaderT r m) where
  local = withReaderT

instance monadStateReaderT :: MonadState s m => MonadState s (ReaderT r m) where
  state = lift <<< state

instance monadTellReaderT :: MonadTell w m => MonadTell w (ReaderT r m) where
  tell = lift <<< tell

instance monadWriterReaderT :: MonadWriter w m => MonadWriter w (ReaderT r m) where
  listen = mapReaderT listen
  pass = mapReaderT pass

instance distributiveReaderT :: Distributive g => Distributive (ReaderT e g) where
  distribute a = ReaderT \e -> collect (\r -> case r of ReaderT r' -> r' e) a
  collect f = distribute <<< map f

instance monadRecReaderT :: MonadRec m => MonadRec (ReaderT r m) where
  tailRecM k a = ReaderT \r -> tailRecM (k' r) a
    where
    k' r a' = case k a' of ReaderT f -> pure =<< f r
