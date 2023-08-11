module Pipes.ListT where

import Prelude hiding (discard)

import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative)
import Control.Monad.Except.Trans (ExceptT, runExceptT, class MonadError, class MonadTrans, lift, catchError, class MonadThrow, throwError)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, local, ask)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter, listen, pass, tell)
import Control.Plus (class Plus, empty)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Effect.Class (class MonadEffect, liftEffect)
import Pipes (discard, for, yield)
import Pipes.Core (Producer, Producer_, runEffect, runEffectRec, (>\\))
import Pipes.Internal (Proxy(Pure, M, Respond, Request))


newtype ListT m a = Select (Producer a m Unit)

enumerate :: forall a m. ListT m a -> Producer a m Unit
enumerate (Select l) = l

runListT :: forall a m. (Monad m) => ListT m a -> m Unit
runListT l = runEffect (enumerate (l *> empty))

runListTRec :: forall a m. (MonadRec m) => ListT m a -> m Unit
runListTRec l = runEffectRec (enumerate (l *> empty))

every :: forall a m t. Monad m => Enumerable t => t m a -> Producer_ a m Unit
every it = discard >\\ enumerate (toListT it)

instance listTFunctor :: (Monad m) => Functor (ListT m) where
    map f (Select p) = Select (for p (yield <<< f))

instance listTApply :: (Monad m) => Apply (ListT m) where
    apply (Select mf) (Select mx) = Select (for mf (\f -> for mx (\x -> yield (f x))))

instance listTApplicative :: (Monad m) => Applicative (ListT m) where
    pure = Select <<< yield

instance listTBind :: (Monad m) => Bind (ListT m) where
    bind (Select p1) f = Select (for p1 (enumerate <<< f))

instance listTMonad :: (Monad m) => Monad (ListT m)

instance listTMonadTrans :: MonadTrans ListT where
    lift m = Select (lift m >>= yield)

instance listTAlt :: (Monad m) => Alt (ListT m) where
    alt (Select p1) (Select p2) = Select (p1 *> p2)

instance listTPlus :: (Monad m) => Plus (ListT m) where
    empty = Select (pure unit)

instance listTAlternative :: (Monad m) => Alternative (ListT m)

-- XXX: these won't compile
-- instance listTMonadPlus :: (Monad m) => MonadPlus (ListT m)
-- instance listTMonadZero :: (Monad m) => MonadZero (ListT m)

instance listTMonadEffect :: (MonadEffect m) => MonadEffect (ListT m) where
    liftEffect = lift <<< liftEffect

instance listTSemigroup :: (Monad m) => Semigroup (ListT m a) where
    append = alt

instance listTMonoid :: (Monad m) => Monoid (ListT m a) where
    mempty = empty

instance listTMonadState :: (MonadState s m) => MonadState s (ListT m) where
    state = lift <<< state

instance listTMonadTell :: (Monoid w, MonadTell w m) => MonadTell w (ListT m) where
    tell = lift <<< tell

instance listTMonadWriter :: (Monoid w, MonadWriter w m) => MonadWriter w (ListT m) where
    listen (Select p) = Select (go p mempty)
        where
        go (Request a' fa) w = Request a' (\a -> go (fa a) w)
        go (Respond b fb') w = Respond (Tuple b w) (\b' -> go (fb' b') w)
        go (M m)           w = M (do
                                    Tuple p' w' <- listen m
                                    pure (go p' (append w w')))
        go (Pure r)        _ = Pure r

    pass (Select p) = Select (go p mempty)
        where
        go (Request a' fa)           w = Request a' (\a -> go (fa a) w)
        go (Respond (Tuple b f) fb') w = M (pass (pure (Tuple _1 _2)))
                                         where _1 = Respond b (\b' -> go (fb' b') (f w))
                                               _2 = \_ -> f w
        go (M m)                     w = M (do Tuple p' w' <- listen m
                                               pure (go p' (append w w')))
        go (Pure r)                  _ = Pure r

instance listTMonadAsk :: (MonadAsk r m) => MonadAsk r (ListT m) where
    ask = lift ask

instance listTMonadReader :: (MonadReader r m) => MonadReader r (ListT m) where
    local f (Select l) = Select (local f l)

instance listTMonadThrow :: (MonadThrow e m) => MonadThrow e (ListT m) where
    throwError = lift <<< throwError

instance listTMonadError :: (MonadError e m) => MonadError e (ListT m) where
    catchError (Select l) f = Select (l `catchError` (enumerate <<< f))

class Enumerable t where
    toListT :: forall a m. Monad m => t m a -> ListT m a

instance listTEnumerable :: Enumerable ListT where
    toListT = identity

instance maybeTEnumerable :: Enumerable MaybeT where
    toListT m = Select $ do
        x <- lift $ runMaybeT m
        case x of
            Nothing -> pure unit
            Just a  -> yield a

instance errorTEnumerable :: Enumerable (ExceptT e) where
    toListT m = Select $ do
        x <- lift $ runExceptT m
        case x of
            Left  _ -> pure unit
            Right a -> yield a
