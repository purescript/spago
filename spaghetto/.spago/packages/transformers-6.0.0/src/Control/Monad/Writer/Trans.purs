-- | This module defines the writer monad transformer, `WriterT`.

module Control.Monad.Writer.Trans
  ( WriterT(..), runWriterT, execWriterT, mapWriterT
  , module Control.Monad.Trans.Class
  , module Control.Monad.Writer.Class
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, catchError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, tell, class MonadWriter, censor, listen, listens, pass)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus, empty)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), snd)
import Effect.Class (class MonadEffect, liftEffect)

-- | The writer monad transformer.
-- |
-- | This monad transformer extends the base monad with a monoidal accumulator of
-- | type `w`.
-- |
-- | The `MonadWriter` type class describes the operations supported by this monad.
newtype WriterT w m a = WriterT (m (Tuple a w))

-- | Run a computation in the `WriterT` monad.
runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)
runWriterT (WriterT x) = x

-- | Run a computation in the `WriterT` monad, discarding the result.
execWriterT :: forall w m a. Functor m => WriterT w m a -> m w
execWriterT (WriterT m) = snd <$> m

-- | Change the accumulator and base monad types in a `WriterT` monad action.
mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (Tuple a w1) -> m2 (Tuple b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b
mapWriterT f (WriterT m) = WriterT (f m)

derive instance newtypeWriterT :: Newtype (WriterT w m a) _

instance functorWriterT :: Functor m => Functor (WriterT w m) where
  map f = mapWriterT $ map \(Tuple a w) -> Tuple (f a) w

instance applyWriterT :: (Semigroup w, Apply m) => Apply (WriterT w m) where
  apply (WriterT f) (WriterT v) = WriterT
    let k (Tuple a w) (Tuple b w') = Tuple (a b) (w <> w')
    in k <$> f <*> v

instance applicativeWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure a = WriterT $ pure $ Tuple a mempty

instance altWriterT :: Alt m => Alt (WriterT w m) where
  alt (WriterT m) (WriterT n) = WriterT (m <|> n)

instance plusWriterT :: Plus m => Plus (WriterT w m) where
  empty = WriterT empty

instance alternativeWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m)

instance bindWriterT :: (Semigroup w, Bind m) => Bind (WriterT w m) where
  bind (WriterT m) k = WriterT $
    m >>= \(Tuple a w) ->
      case k a of
        WriterT wt ->
          map (\(Tuple b w') -> Tuple b (w <> w')) wt

instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)

instance monadRecWriterT :: (Monoid w, MonadRec m) => MonadRec (WriterT w m) where
  tailRecM f a = WriterT $ tailRecM f' (Tuple a mempty)
    where
    f' (Tuple a' w) =
      case f a' of
        WriterT wt -> wt >>= \(Tuple m w1) ->
          pure case m of
            Loop x -> Loop (Tuple x (w <> w1))
            Done y -> Done (Tuple y (w <> w1))

instance monadPlusWriterT :: (Monoid w, MonadPlus m) => MonadPlus (WriterT w m)

instance monadTransWriterT :: Monoid w => MonadTrans (WriterT w) where
  lift m = WriterT do
    a <- m
    pure $ Tuple a mempty

instance monadEffectWriter :: (Monoid w, MonadEffect m) => MonadEffect (WriterT w m) where
  liftEffect = lift <<< liftEffect

instance monadContWriterT :: (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
  callCC f = WriterT $ callCC \c ->
    case f (\a -> WriterT $ c (Tuple a mempty)) of WriterT b -> b

instance monadThrowWriterT :: (Monoid w, MonadThrow e m) => MonadThrow e (WriterT w m) where
  throwError e = lift (throwError e)

instance monadErrorWriterT :: (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
  catchError (WriterT m) h = WriterT $ catchError m (\e -> case h e of WriterT a -> a)

instance monadAskWriterT :: (Monoid w, MonadAsk r m) => MonadAsk r (WriterT w m) where
  ask = lift ask

instance monadReaderWriterT :: (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  local f = mapWriterT (local f)

instance monadStateWriterT :: (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  state f = lift (state f)

instance monadTellWriterT :: (Monoid w, Monad m) => MonadTell w (WriterT w m) where
  tell = WriterT <<< pure <<< Tuple unit

instance monadWriterWriterT :: (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  listen (WriterT m) = WriterT do
    Tuple a w <- m
    pure $ Tuple (Tuple a w) w
  pass (WriterT m) = WriterT do
    Tuple (Tuple a f) w <- m
    pure $ Tuple a (f w)

instance semigroupWriterT :: (Apply m, Semigroup w, Semigroup a) => Semigroup (WriterT w m a) where
  append = lift2 (<>)

instance monoidWriterT :: (Applicative m, Monoid w, Monoid a) => Monoid (WriterT w m a) where
  mempty = pure mempty

