-- | This module defines the list monad transformer, `ListT`.

module Control.Monad.List.Trans
  ( ListT(..)
  , Step(..)
  , catMaybes
  , cons
  , drop
  , dropWhile
  , filter
  , foldl
  , foldlRec
  , foldl'
  , foldlRec'
  , fromEffect
  , head
  , iterate
  , mapMaybe
  , nil
  , prepend
  , prepend'
  , repeat
  , runListT
  , runListTRec
  , scanl
  , singleton
  , tail
  , take
  , takeWhile
  , uncons
  , unfold
  , wrapEffect
  , wrapLazy
  , zipWith
  , zipWith'
  , module Control.Monad.Trans.Class
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Rec.Class as MR
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1)
import Effect.Class (class MonadEffect, liftEffect)

-- | The list monad transformer.
-- |
-- | This monad transformer extends the base monad with _non-determinism_.
-- | That is, the transformed monad supports the same effects as the base monad
-- | but with multiple return values.
newtype ListT f a = ListT (f (Step a (ListT f a)))

-- | The result of a single step in a `ListT` computation. Either:
-- |
-- | - Computation has finished (`Done`), or
-- | - A result has been returned, along with the next part of the computation (`Yield`).
-- |
-- | The `Skip` constructor allows us to avoid traversing lists during certain operations.
data Step a s
  = Yield a (Lazy s)
  | Skip (Lazy s)
  | Done

-- | Drain a `ListT`, running it to completion and discarding all values.
runListT :: forall f a. Monad f => ListT f a -> f Unit
runListT = foldl' (\_ _ -> pure unit) unit

-- | Drain a `ListT`, running it to completion and discarding all values.
-- | Stack safe: Uses tail call optimization.
runListTRec :: forall f a. MR.MonadRec f => ListT f a -> f Unit
runListTRec = foldlRec' (\_ _ -> pure unit) unit

-- | The empty list.
nil :: forall f a. Applicative f => ListT f a
nil = ListT $ pure Done

-- | Attach an element to the front of a list.
cons :: forall f a. Applicative f => Lazy a -> Lazy (ListT f a) -> ListT f a
cons lh t = ListT $ pure $ Yield (force lh) t

-- | Prepend an element to a lazily-evaluated list.
prepend' :: forall f a. Applicative f => a -> Lazy (ListT f a) -> ListT f a
prepend' h t = ListT $ pure (Yield h t)

-- | Prepend an element to a list.
prepend :: forall f a. Applicative f => a -> ListT f a -> ListT f a
prepend h t = prepend' h (defer $ const t)

-- | Lift a computation on list steps to a computation on whole lists.
stepMap :: forall f a b. Functor f => (Step a (ListT f a) -> Step b (ListT f b)) -> ListT f a -> ListT f b
stepMap f (ListT l) = ListT $ f <$> l

-- | Append one list to another.
concat :: forall f a. Applicative f => ListT f a -> ListT f a -> ListT f a
concat x y = stepMap f x where
  f (Yield a s) = Yield a ((_ <> y) <$> s)
  f (Skip s)    = Skip ((_ <> y) <$> s)
  f Done        = Skip (defer $ const y)

-- | Create a list with one element.
singleton :: forall f a. Applicative f => a -> ListT f a
singleton a = prepend a nil

-- | Lift a computation from the base functor.
fromEffect :: forall f a. Applicative f => f a -> ListT f a
fromEffect fa = ListT $ (flip Yield (defer $ \_ -> nil)) <$> fa

-- | Lift a computation from the base monad.
wrapEffect :: forall f a. Functor f => f (ListT f a) -> ListT f a
wrapEffect v = ListT $ Skip <<< defer <<< const <$> v

-- | Defer evaluation of a list.
wrapLazy :: forall f a. Applicative f => Lazy (ListT f a) -> ListT f a
wrapLazy v = ListT $ pure (Skip v)

-- | Unfold a list using an effectful generator function.
unfold :: forall f a z. Monad f => (z -> f (Maybe (Tuple z a))) -> z -> ListT f a
unfold f z = ListT $ g <$> f z
  where
  g (Just (Tuple z' a)) = Yield a (defer \_ -> unfold f z')
  g Nothing            = Done

-- | Generate an infinite list by iterating a function.
iterate :: forall f a. Monad f => (a -> a) -> a -> ListT f a
iterate f a = unfold g a
  where
  g x = pure $ Just (Tuple (f x) x)

-- | Generate an infinite list by repeating a value.
repeat :: forall f a. Monad f => a -> ListT f a
repeat = iterate identity

-- | Take a number of elements from the front of a list.
take :: forall f a. Applicative f => Int -> ListT f a -> ListT f a
take 0 _ = nil
take n fa = stepMap f fa where
  f (Yield a s) = Yield a (take (n - 1) <$> s)
  f (Skip s)    = Skip (take n <$> s)
  f Done        = Done

-- | Take elements from the front of a list while a predicate holds.
takeWhile :: forall f a. Applicative f => (a -> Boolean) -> ListT f a -> ListT f a
takeWhile f = stepMap g where
  g (Yield a s) = if f a then Yield a (takeWhile f <$> s) else Done
  g (Skip s)    = Skip $ takeWhile f <$> s
  g Done        = Done

-- | Drop a number of elements from the front of a list.
drop :: forall f a. Applicative f => Int -> ListT f a -> ListT f a
drop 0 fa = fa
drop n fa = stepMap f fa where
  f (Yield _ s) = Skip (drop (n - 1) <$> s)
  f (Skip s)    = Skip (drop n <$> s)
  f Done        = Done

-- | Drop elements from the front of a list while a predicate holds.
dropWhile :: forall f a. Applicative f => (a -> Boolean) -> ListT f a -> ListT f a
dropWhile f = stepMap g where
  g (Yield a s) = if f a then Skip (dropWhile f <$> s) else Yield a s
  g (Skip s)    = Skip $ dropWhile f <$> s
  g Done        = Done

-- | Remove elements from a list for which a predicate fails to hold.
filter :: forall f a. Functor f => (a -> Boolean) -> ListT f a -> ListT f a
filter f = stepMap g where
  g (Yield a s) = if f a then Yield a s' else Skip s' where s' = filter f <$> s
  g (Skip s)    = Skip s' where s' = filter f <$> s
  g Done        = Done

-- | Apply a function to the elements of a list, keeping only those return values which contain a result.
mapMaybe :: forall f a b. Functor f => (a -> Maybe b) -> ListT f a -> ListT f b
mapMaybe f = stepMap g where
  g (Yield a s) = (fromMaybe Skip (Yield <$> (f a))) (mapMaybe f <$> s)
  g (Skip s)    = Skip $ mapMaybe f <$> s
  g Done        = Done

-- | Remove elements from a list which do not contain a value.
catMaybes :: forall f a. Functor f => ListT f (Maybe a) -> ListT f a
catMaybes = mapMaybe identity

-- | Perform the first step of a computation in the `ListT` monad.
uncons :: forall f a. Monad f => ListT f a -> f (Maybe (Tuple a (ListT f a)))
uncons (ListT l) = l >>= g
  where
  g (Yield a s) = pure $ Just $ Tuple a (force s)
  g (Skip s)    = uncons (force s)
  g Done        = pure Nothing

-- | Extract the first element of a list.
head :: forall f a. Monad f => ListT f a -> f (Maybe a)
head l = ((<$>) fst) <$> uncons l

-- | Extract all but the first element of a list.
tail :: forall f a. Monad f => ListT f a -> f (Maybe (ListT f a))
tail l = ((<$>) snd) <$> uncons l

-- | Fold a list from the left, accumulating the result (effectfully) using the specified function.
foldl' :: forall f a b. Monad f => (b -> a -> f b) -> b -> ListT f a -> f b
foldl' f = loop where
  loop b l = uncons l >>= g
    where
    g Nothing             = pure b
    g (Just (Tuple a as)) = (f b a) >>= (flip loop as)

-- | Fold a list from the left, accumulating the result (effectfully) using the specified function.
-- | Uses tail call optimization.
foldlRec' :: forall f a b. MR.MonadRec f => (b -> a -> f b) -> b -> ListT f a -> f b
foldlRec' f = MR.tailRecM2 loop where
  loop b l = uncons l >>= g
    where
    g Nothing             = pure (MR.Done b)
    g (Just (Tuple a as)) = (f b a) >>= \b' -> pure (MR.Loop {a: b', b: as})

-- | Fold a list from the left, accumulating the result using the specified function.
foldl :: forall f a b. Monad f => (b -> a -> b) -> b -> ListT f a -> f b
foldl f = loop where
  loop b l = uncons l >>= g
    where
    g Nothing             = pure b
    g (Just (Tuple a as)) = loop (f b a) as

-- | Fold a list from the left, accumulating the result using the specified function.
-- | Uses tail call optimization.
foldlRec :: forall f a b. MR.MonadRec f => (b -> a -> b) -> b -> ListT f a -> f b
foldlRec f = MR.tailRecM2 loop
  where
    loop b l = uncons l >>= g
      where
      g Nothing             = pure (MR.Done b)
      g (Just (Tuple a as)) = pure (MR.Loop {a: f b a, b: as})

-- | Fold a list from the left, accumulating the list of results using the specified function.
scanl :: forall f a b. Monad f => (b -> a -> b) -> b -> ListT f a -> ListT f b
scanl f b l = unfold g (Tuple b l)
  where
  g (Tuple b' (ListT l')) = h <$> l'
    where
    h (Yield a s) = let b'' = f b' a in Just $ Tuple (Tuple b'' (force s)) b'
    h (Skip s)    = Just $ Tuple (Tuple b' (force s)) b'
    h Done        = Nothing

-- | Zip the elements of two lists, combining elements at the same position from each list.
zipWith' :: forall f a b c. Monad f => (a -> b -> f c) -> ListT f a -> ListT f b -> ListT f c
zipWith' f = loop where
  loop fa fb = wrapEffect do
    ua <- uncons fa
    ub <- uncons fb
    g ua ub

  g _ Nothing = pure nil
  g Nothing _ = pure nil
  g (Just (Tuple ha ta)) (Just (Tuple hb tb)) = (flip prepend') (defer \_ -> zipWith' f ta tb) <$> (f ha hb)

-- | Zip the elements of two lists, combining elements at the same position from each list.
zipWith :: forall f a b c. Monad f => (a -> b -> c) -> ListT f a -> ListT f b -> ListT f c
zipWith f = zipWith' g
  where
  g a b = pure $ f a b

derive instance newtypeListT :: Newtype (ListT f a) _

instance semigroupListT :: Applicative f => Semigroup (ListT f a) where
  append = concat

instance monoidListT :: Applicative f => Monoid (ListT f a) where
  mempty = nil

instance functorListT :: Functor f => Functor (ListT f) where
  map f = stepMap g where
    g (Yield a s) = Yield (f a) ((<$>) f <$> s)
    g (Skip s)    = Skip ((<$>) f <$> s)
    g Done        = Done

instance unfoldableListT :: Monad f => Unfoldable (ListT f) where
  unfoldr f b = go (f b)
    where
      go = case _ of
        Nothing -> nil
        Just (Tuple x y) -> cons (pure x) (defer \_ -> (go (f y)))

instance unfoldable1ListT :: Monad f => Unfoldable1 (ListT f) where
  unfoldr1 f b = go (f b)
    where
      go = case _ of
        Tuple x Nothing -> singleton x
        Tuple x (Just y) -> cons (pure x) (defer \_ -> (go (f y)))

instance applyListT :: Monad f => Apply (ListT f) where
  apply = ap

instance applicativeListT :: Monad f => Applicative (ListT f) where
  pure = singleton

instance bindListT :: Monad f => Bind (ListT f) where
  bind fa f = stepMap g fa where
    g (Yield a s) = Skip (h <$> s)
      where
      h s' = f a <> (s' >>= f)
    g (Skip s) = Skip ((_ >>= f) <$> s)
    g Done = Done

instance monadListT :: Monad f => Monad (ListT f)

instance monadTransListT :: MonadTrans ListT where
  lift = fromEffect

instance altListT :: Applicative f => Alt (ListT f) where
  alt = concat

instance plusListT :: Monad f => Plus (ListT f) where
  empty = nil

instance alternativeListT :: Monad f => Alternative (ListT f)

instance monadPlusListT :: Monad f => MonadPlus (ListT f)

instance monadEffectListT :: MonadEffect m => MonadEffect (ListT m) where
  liftEffect = lift <<< liftEffect
