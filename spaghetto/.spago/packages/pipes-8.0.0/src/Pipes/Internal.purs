module Pipes.Internal where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Monad.Except.Trans (class MonadError, catchError, class MonadThrow, throwError)
import Control.Monad.Morph (class MFunctor, class MMonad)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, local, ask)
import Control.Monad.Rec.Class (class MonadRec, Step(Done, Loop))
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadWriter, class MonadTell, listen, pass, tell)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus, empty)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

data Proxy a' a b' b m r
  = Request a' (a  -> Proxy a' a b' b m r)
  | Respond b  (b' -> Proxy a' a b' b m r)
  | M          (m    (Proxy a' a b' b m r))
  | Pure       r

instance functorProxy :: (Monad m) => Functor (Proxy a' a b' b m) where
  map f p0 = go p0 where
    go p = case p of
      Request a' fa  -> Request a' \x -> go (fa x)
      Respond b  fb' -> Respond b  \x -> go (fb' x)
      M           m  -> M          (m >>= \v -> pure (go v))
      Pure        r  -> Pure       (f r)

instance applyProxy :: (Monad m) => Apply (Proxy a' a b' b m) where
  apply pf0 px = go pf0 where
    go pf = case pf of
      Request a' fa  -> Request a' \x -> go (fa x)
      Respond b  fb' -> Respond b  \x -> go (fb' x)
      M           m  -> M          (m >>= \v -> pure (go v))
      Pure        f  -> f <$> px

instance applicativeProxy :: (Monad m) => Applicative (Proxy a' a b' b m) where
  pure = Pure

instance bindProxy :: (Monad m) => Bind (Proxy a' a b' b m) where
  bind p0 f = go p0 where
    go p = case p of
      Request a' fa  -> Request a' \x -> go (fa x)
      Respond b  fb' -> Respond b  \x -> go (fb' x)
      M           m  -> M          (m >>= \v -> pure (go v))
      Pure        r  -> f r

instance monadProxy :: (Monad m) => Monad (Proxy a' a b' b m)

instance monoidProxy :: (Monad m, Monoid r) => Monoid (Proxy a' a b' b m r) where
  mempty = Pure mempty

instance semigroupProxy :: (Monad m, Semigroup r) => Semigroup (Proxy a' a b' b m r) where
  append p1 p2 = go p1 where
    go p = case p of
      Request a' fa  -> Request a' \x -> go (fa x)
      Respond b  fb' -> Respond b  \x -> go (fb' x)
      M           m  -> M          (m >>= \v -> pure (go v))
      Pure       r1  -> (r1 <> _) <$> p2

instance monadTransProxy :: MonadTrans (Proxy a' a b' b) where
  lift m = M (Pure <$> m)

instance proxyMFunctor :: MFunctor (Proxy a' a b' b) where
    hoist nat p0 = go (observe p0)
      where
        go p = case p of
            Request a' fa  -> Request a' \x -> go (fa  x)
            Respond b  fb' -> Respond b  \x -> go (fb' x)
            M          m   -> M (nat (m >>= \p' -> pure (go p')))
            Pure    r      -> Pure r

instance proxyMMonad :: MMonad (Proxy a' a b' b) where
    embed f = go
      where
        go p = case p of
            Request a' fa  -> Request a' \x -> go (fa  x)
            Respond b  fb' -> Respond b  \x -> go (fb' x)
            M          m   -> f m >>= go
            Pure    r      -> Pure r

instance proxyMonadEffect :: MonadEffect m => MonadEffect (Proxy a' a b' b m) where
    liftEffect m = M (liftEffect (m >>= \r -> pure (Pure r)))

instance proxyMonadAff :: MonadAff m => MonadAff (Proxy a' a b' b m) where
    liftAff m = M (liftAff (m >>= \r -> pure (Pure r)))

instance proxyMonadAsk :: MonadAsk r m => MonadAsk r (Proxy a' a b' b m) where
    ask = lift ask

instance proxyMonadReader :: MonadReader r m => MonadReader r (Proxy a' a b' b m) where
    local f = go
        where
          go p = case p of
              Request a' fa  -> Request a' (\a  -> go (fa  a ))
              Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
              Pure    r      -> Pure r
              M       m      -> M (local f m >>= \r -> pure (go r))

instance proxyMonadState :: MonadState s m => MonadState s (Proxy a' a b' b m) where
    state = lift <<< state

instance proxyMonadTell :: (Monoid w, MonadTell w m) => MonadTell w (Proxy a' a b' b m) where
    tell = lift <<< tell

instance proxyMonadWriter :: (Monoid w, MonadWriter w m) => MonadWriter w (Proxy a' a b' b m) where
    listen p0 = go p0 mempty
        where
        go p w = case p of
            Request a' fa -> Request a' (\a  -> go (fa  a ) w)
            Respond b fb' -> Respond b  (\b' -> go (fb' b') w)
            Pure r        -> Pure (Tuple r w)
            M m           -> M (do
                                Tuple p' w' <- listen m
                                pure (go p' (append w w')))

    pass p0 = go p0 mempty
        where
        go p w = case p of
            Request a' fa    -> Request a' (\a  -> go (fa  a ) w)
            Respond b fb'    -> Respond b  (\b' -> go (fb' b') w)
            Pure (Tuple r f) -> M (pass (pure (Tuple (Pure r) \_ -> f w)))
            M m              -> M (do
                                    Tuple p' w' <- listen m
                                    pure (go p' (append w w')))

instance proxyAlt :: (MonadPlus m) => Alt (Proxy a' a b' b m) where
    alt (Request a' fa) p = Request a' (\a  -> (fa a) <|> p)
    alt (Respond b fb') p = Respond b  (\b' -> (fb' b') <|> p)
    alt (Pure r)        _ = Pure r
    alt (M m)           p = M ((do
                                  p' <- m
                                  pure (p' <|> p)) <|> pure p)

instance proxyPlus :: (MonadPlus m) => Plus (Proxy a' a b' b m) where
    empty = lift empty

instance proxyAlternative :: (MonadPlus m) => Alternative (Proxy a' a b' b m)

-- XXX: these won't compile
-- instance proxyMonadPlus :: (MonadPlus m) => MonadPlus (Proxy a' a b' b m)
-- instance proxyMonadZero :: (MonadZero m) => MonadZero (Proxy a' a b' b m)

instance proxyMonadThrow :: (MonadThrow e m) => MonadThrow e (Proxy a' a b' b m) where
    throwError = lift <<< throwError

instance proxyMonadError :: (MonadError e m) => MonadError e (Proxy a' a b' b m) where
    catchError (Request a' fa) f = Request a' (\a  -> catchError (fa  a ) f)
    catchError (Respond b fb') f = Respond b  (\b' -> catchError (fb' b') f)
    catchError (Pure r)        _ = Pure r
    catchError (M m)           f = M ((do
                                          p' <- m
                                          pure (catchError p' f)) `catchError` (pure <<< f))

instance monadRecProxy :: Monad m => MonadRec (Proxy a' a b' b m) where
  tailRecM f a0 = go (f a0)
    where
    go = case _ of
      Pure (Loop a)  -> go (f a)
      Pure (Done b)  -> Pure b
      M m            -> M $ m >>= \v -> pure (go v)
      Request a' fa  -> Request a' \x -> go (fa x)
      Respond b  fb' -> Respond b  \x -> go (fb' x)

observe :: forall m a' a b' b r
        .  Monad m => Proxy a' a b' b m r -> Proxy a' a b' b m r
observe p0 = M (go p0) where
    go p = case p of
        Request a' fa  -> pure (Request a' (observe <<< fa))
        Respond b  fb' -> pure (Respond b  (observe <<< fb'))
        M           m  -> m >>= go
        Pure        r  -> pure (Pure r)

newtype X = X X

closed :: forall a. X -> a
closed (X x) = closed x
