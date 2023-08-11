-- | This module defines the `MaybeT` monad transformer.

module Control.Monad.Maybe.Trans
  ( MaybeT(..), runMaybeT, mapMaybeT
  , module Control.Monad.Trans.Class
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, catchError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadWriter, class MonadTell, pass, listen, tell)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)

-- | The `MaybeT` monad transformer.
-- |
-- | This monad transformer extends the base monad, supporting failure and alternation via
-- | the `MonadPlus` type class.
newtype MaybeT m a = MaybeT (m (Maybe a))

-- | Run a computation in the `MaybeT` monad.
runMaybeT :: forall m a. MaybeT m a -> m (Maybe a)
runMaybeT (MaybeT x) = x

-- | Change the result type of a `MaybeT` monad action.
mapMaybeT :: forall m1 m2 a b. (m1 (Maybe a) -> m2 (Maybe b)) -> MaybeT m1 a -> MaybeT m2 b
mapMaybeT f (MaybeT m) = MaybeT (f m)

derive instance newtypeMaybeT :: Newtype (MaybeT m a) _

instance functorMaybeT :: Functor m => Functor (MaybeT m) where
  map f (MaybeT ma) = MaybeT (map f <$> ma)

instance applyMaybeT :: Monad m => Apply (MaybeT m) where
  apply = ap

instance applicativeMaybeT :: Monad m => Applicative (MaybeT m) where
  pure = MaybeT <<< pure <<< Just

instance bindMaybeT :: Monad m => Bind (MaybeT m) where
  bind (MaybeT x) f = MaybeT do
    x >>= case _ of
      Nothing -> pure Nothing
      Just y -> case f y of MaybeT m -> m

instance monadMaybeT :: Monad m => Monad (MaybeT m)

instance monadTransMaybeT :: MonadTrans MaybeT where
  lift = MaybeT <<< liftM1 Just

instance altMaybeT :: Monad m => Alt (MaybeT m) where
  alt (MaybeT m1) (MaybeT m2) = MaybeT do
    m <- m1
    case m of
      Nothing -> m2
      ja -> pure ja

instance plusMaybeT :: Monad m => Plus (MaybeT m) where
  empty = MaybeT (pure Nothing)

instance alternativeMaybeT :: Monad m => Alternative (MaybeT m)

instance monadPlusMaybeT :: Monad m => MonadPlus (MaybeT m)

instance monadRecMaybeT :: MonadRec m => MonadRec (MaybeT m) where
  tailRecM f =
    MaybeT <<< tailRecM \a ->
      case f a of
        MaybeT m -> m >>= \m' ->
          pure case m' of
            Nothing -> Done Nothing
            Just (Loop a1) -> Loop a1
            Just (Done b) -> Done (Just b)

instance monadEffectMaybe :: MonadEffect m => MonadEffect (MaybeT m) where
  liftEffect = lift <<< liftEffect

instance monadContMaybeT :: MonadCont m => MonadCont (MaybeT m) where
  callCC f =
    MaybeT $ callCC \c -> case f (\a -> MaybeT $ c $ Just a) of MaybeT m -> m

instance monadThrowMaybeT :: MonadThrow e m => MonadThrow e (MaybeT m) where
  throwError e = lift (throwError e)

instance monadErrorMaybeT :: MonadError e m => MonadError e (MaybeT m) where
  catchError (MaybeT m) h =
    MaybeT $ catchError m (\a -> case h a of MaybeT b -> b)

instance monadAskMaybeT :: MonadAsk r m => MonadAsk r (MaybeT m) where
  ask = lift ask

instance monadReaderMaybeT :: MonadReader r m => MonadReader r (MaybeT m) where
  local f = mapMaybeT (local f)

instance monadStateMaybeT :: MonadState s m => MonadState s (MaybeT m) where
  state f = lift (state f)

instance monadTellMaybeT :: MonadTell w m => MonadTell w (MaybeT m) where
  tell = lift <<< tell

instance monadWriterMaybeT :: MonadWriter w m => MonadWriter w (MaybeT m) where
  listen = mapMaybeT \m -> do
    Tuple a w <- listen m
    pure $ (\r -> Tuple r w) <$> a
  pass = mapMaybeT \m -> pass do
    a <- m
    pure case a of
      Nothing -> Tuple Nothing identity
      Just (Tuple v f) -> Tuple (Just v) f

instance semigroupMaybeT :: (Monad m, Semigroup a) => Semigroup (MaybeT m a) where
  append = lift2 (<>)

instance monoidMaybeT :: (Monad m, Monoid a) => Monoid (MaybeT m a) where
  mempty = pure mempty

