-- | This module defines the state monad transformer, `StateT`.

module Control.Monad.State.Trans
  ( StateT(..), runStateT, evalStateT, execStateT, mapStateT, withStateT
  , module Control.Monad.Trans.Class
  , module Control.Monad.State.Class
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, catchError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState, get, gets, modify, modify_, put, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadWriter, class MonadTell, pass, listen, tell)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus, empty)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (class MonadEffect, liftEffect)

-- | The state monad transformer.
-- |
-- | This monad transformer extends the base monad with the operations `get`
-- | and `put` which can be used to model a single piece of mutable state.
-- |
-- | The `MonadState` type class describes the operations supported by this monad.
newtype StateT s m a = StateT (s -> m (Tuple a s))

-- | Run a computation in the `StateT` monad.
runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT s) = s

-- | Run a computation in the `StateT` monad, discarding the final state.
evalStateT :: forall s m a. Functor m => StateT s m a -> s -> m a
evalStateT (StateT m) s = fst <$> m s

-- | Run a computation in the `StateT` monad discarding the result.
execStateT :: forall s m a. Functor m => StateT s m a -> s -> m s
execStateT (StateT m) s = snd <$> m s

-- | Change the result type in a `StateT` monad action.
mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b
mapStateT f (StateT m) = StateT (f <<< m)

-- | Modify the final state in a `StateT` monad action.
withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
withStateT f (StateT s) = StateT (s <<< f)

derive instance newtypeStateT :: Newtype (StateT s m a) _

instance functorStateT :: Functor m => Functor (StateT s m) where
  map f (StateT a) = StateT (\s -> map (\(Tuple b s') -> Tuple (f b) s') (a s))

instance applyStateT :: Monad m => Apply (StateT s m) where
  apply = ap

instance applicativeStateT :: Monad m => Applicative (StateT s m) where
  pure a = StateT \s -> pure $ Tuple a s

instance altStateT :: (Monad m, Alt m) => Alt (StateT s m) where
  alt (StateT x) (StateT y) = StateT \s -> x s <|> y s

instance plusStateT :: (Monad m, Plus m) => Plus (StateT s m) where
  empty = StateT \_ -> empty

instance alternativeStateT :: (Monad m, Alternative m) => Alternative (StateT s m)

instance bindStateT :: Monad m => Bind (StateT s m) where
  bind (StateT x) f = StateT \s ->
    x s >>= \(Tuple v s') -> case f v of StateT st -> st s'

instance monadStateT :: Monad m => Monad (StateT s m)

instance monadRecStateT :: MonadRec m => MonadRec (StateT s m) where
  tailRecM f a = StateT \s -> tailRecM f' (Tuple a s)
    where
    f' (Tuple a' s) =
      case f a' of
        StateT st -> st s >>= \(Tuple m s1) ->
          pure case m of
            Loop x -> Loop (Tuple x s1)
            Done y -> Done (Tuple y s1)

instance monadPlusStateT :: MonadPlus m => MonadPlus (StateT s m)

instance monadTransStateT :: MonadTrans (StateT s) where
  lift m = StateT \s -> do
    x <- m
    pure $ Tuple x s

instance lazyStateT :: Lazy (StateT s m a) where
  defer f = StateT \s -> case f unit of StateT f' -> f' s

instance monadEffectState :: MonadEffect m => MonadEffect (StateT s m) where
  liftEffect = lift <<< liftEffect

instance monadContStateT :: MonadCont m => MonadCont (StateT s m) where
  callCC f = StateT \s -> callCC \c ->
    case f (\a -> StateT \s' -> c (Tuple a s')) of StateT f' -> f' s

instance monadThrowStateT :: MonadThrow e m => MonadThrow e (StateT s m) where
  throwError e = lift (throwError e)

instance monadErrorStateT :: MonadError e m => MonadError e (StateT s m) where
  catchError (StateT m) h =
    StateT \s -> catchError (m s) (\e -> case h e of StateT f -> f s)

instance monadAskStateT :: MonadAsk r m => MonadAsk r (StateT s m) where
  ask = lift ask

instance monadReaderStateT :: MonadReader r m => MonadReader r (StateT s m) where
  local = mapStateT <<< local

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
  state f = StateT $ pure <<< f

instance monadTellStateT :: MonadTell w m => MonadTell w (StateT s m) where
  tell = lift <<< tell

instance monadWriterStateT :: MonadWriter w m => MonadWriter w (StateT s m) where
  listen m = StateT \s ->
    case m of
      StateT m' -> do
        Tuple (Tuple a s') w <- listen (m' s)
        pure $ Tuple (Tuple a w) s'
  pass m = StateT \s -> pass
    case m of
      StateT m' -> do
        Tuple (Tuple a f) s' <- m' s
        pure $ Tuple (Tuple a s') f

instance semigroupStateT :: (Monad m, Semigroup a) => Semigroup (StateT s m a) where
  append = lift2 (<>)

instance monoidStateT :: (Monad m, Monoid a) => Monoid (StateT s m a) where
  mempty = pure mempty

