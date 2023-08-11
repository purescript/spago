-- | This module defines the _exception monad transformer_ `ExceptT`.

module Control.Monad.Except.Trans
  ( ExceptT(..), runExceptT, withExceptT, mapExceptT, except
  , module Control.Monad.Trans.Class
  , module Control.Monad.Error.Class
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError, catchError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadWriter, class MonadTell, pass, listen, tell)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)

-- | A monad transformer which adds exceptions to other monads, in the same way
-- | as `Except`. As before, `e` is the type of exceptions, and `a` is the type
-- | of successful results. The new type parameter `m` is the inner monad that
-- | computations run in.
newtype ExceptT e m a = ExceptT (m (Either e a))

-- | The inverse of `ExceptT`. Run a computation in the `ExceptT` monad.
runExceptT :: forall e m a. ExceptT e m a -> m (Either e a)
runExceptT (ExceptT x) = x

-- | Transform any exceptions thrown by an `ExceptT` computation using the given function.
withExceptT :: forall e e' m a. Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f (ExceptT t) = ExceptT $ map (mapLeft f) t
  where
  mapLeft _ (Right x) = Right x
  mapLeft f' (Left x) = Left (f' x)

-- | Transform the unwrapped computation using the given function.
mapExceptT :: forall e e' m n a b. (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f (ExceptT m) = ExceptT (f m)

-- | Construct a computation in the `ExceptT` transformer from an `Either` value.
except :: forall e m a. Applicative m => Either e a -> ExceptT e m a
except = ExceptT <<< pure

derive instance newtypeExceptT :: Newtype (ExceptT e m a) _

instance functorExceptT :: Functor m => Functor (ExceptT e m) where
  map f = mapExceptT (map (map f))

instance applyExceptT :: Monad m => Apply (ExceptT e m) where
  apply = ap

instance applicativeExceptT :: Monad m => Applicative (ExceptT e m) where
  pure = ExceptT <<< pure <<< Right

instance bindExceptT :: Monad m => Bind (ExceptT e m) where
  bind (ExceptT m) k =
    ExceptT (m >>= either (pure <<< Left) (\a -> case k a of ExceptT b -> b))

instance monadExceptT :: Monad m => Monad (ExceptT e m)

instance monadRecExceptT :: MonadRec m => MonadRec (ExceptT e m) where
  tailRecM f = ExceptT <<< tailRecM \a ->
    case f a of
      ExceptT m -> m >>= \m' ->
        pure case m' of
          Left e -> Done (Left e)
          Right (Loop a1) -> Loop a1
          Right (Done b) -> Done (Right b)

instance altExceptT :: (Semigroup e, Monad m) => Alt (ExceptT e m) where
  alt (ExceptT m) (ExceptT n) = ExceptT do
    rm <- m
    case rm of
      Right x -> pure (Right x)
      Left err -> do
        rn <- n
        case rn of
          Right x -> pure (Right x)
          Left err' -> pure (Left (err <> err'))

instance plusExceptT :: (Monoid e, Monad m) => Plus (ExceptT e m) where
  empty = throwError (mempty :: e)

instance alternativeExceptT :: (Monoid e, Monad m) => Alternative (ExceptT e m)

instance monadPlusExceptT :: (Monoid e, Monad m) => MonadPlus (ExceptT e m)

instance monadTransExceptT :: MonadTrans (ExceptT e) where
  lift m = ExceptT do
    a <- m
    pure $ Right a

instance monadEffectExceptT :: MonadEffect m => MonadEffect (ExceptT e m) where
  liftEffect = lift <<< liftEffect

instance monadContExceptT :: MonadCont m => MonadCont (ExceptT e m) where
  callCC f = ExceptT $ callCC \c ->
    case f (\a -> ExceptT $ c (Right a)) of ExceptT b -> b

instance monadThrowExceptT :: Monad m => MonadThrow e (ExceptT e m) where
  throwError = ExceptT <<< pure <<< Left

instance monadErrorExceptT :: Monad m => MonadError e (ExceptT e m) where
  catchError (ExceptT m) k =
    ExceptT (m >>= either (\a -> case k a of ExceptT b -> b) (pure <<< Right))

instance monadAskExceptT :: MonadAsk r m => MonadAsk r (ExceptT e m) where
  ask = lift ask

instance monadReaderExceptT :: MonadReader r m => MonadReader r (ExceptT e m) where
  local f = mapExceptT (local f)

instance monadStateExceptT :: MonadState s m => MonadState s (ExceptT e m) where
  state f = lift (state f)

instance monadTellExceptT :: MonadTell w m => MonadTell w (ExceptT e m) where
  tell = lift <<< tell

instance monadWriterExceptT :: MonadWriter w m => MonadWriter w (ExceptT e m) where
  listen = mapExceptT \m -> do
    Tuple a w <- listen m
    pure $ (\r -> Tuple r w) <$> a
  pass = mapExceptT \m -> pass do
    a <- m
    pure case a of
      Left e -> Tuple (Left e) identity
      Right (Tuple r f) -> Tuple (Right r) f

instance semigroupExceptT :: (Monad m, Semigroup a) => Semigroup (ExceptT e m a) where
  append = lift2 (<>)

instance monoidExceptT :: (Monad m, Monoid a) => Monoid (ExceptT e m a) where
  mempty = pure mempty

