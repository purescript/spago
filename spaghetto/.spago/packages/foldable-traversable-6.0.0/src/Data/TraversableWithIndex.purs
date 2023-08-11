module Data.TraversableWithIndex 
  ( class TraversableWithIndex, traverseWithIndex
  , traverseWithIndexDefault
  , forWithIndex
  , scanlWithIndex
  , mapAccumLWithIndex
  , scanrWithIndex
  , mapAccumRWithIndex
  , traverseDefault
  , module Data.Traversable.Accum
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.Functor.App (App(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct (Coproduct(..), coproduct)
import Data.Functor.Product (Product(..), product)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Monoid.Dual (Dual)
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Traversable.Accum (Accum)
import Data.Traversable.Accum.Internal (StateL(..), StateR(..), stateL, stateR)
import Data.Tuple (Tuple(..), curry)


-- | A `Traversable` with an additional index.  
-- | A `TraversableWithIndex` instance must be compatible with its
-- | `Traversable` instance
-- | ```purescript
-- | traverse f = traverseWithIndex (const f)
-- | ```
-- | with its `FoldableWithIndex` instance
-- | ```
-- | foldMapWithIndex f = unwrap <<< traverseWithIndex (\i -> Const <<< f i)
-- | ```
-- | and with its `FunctorWithIndex` instance
-- | ```
-- | mapWithIndex f = unwrap <<< traverseWithIndex (\i -> Identity <<< f i)
-- | ```
-- |
-- | A default implementation is provided by `traverseWithIndexDefault`.
class (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t) <= TraversableWithIndex i t | t -> i where
  traverseWithIndex :: forall a b m. Applicative m => (i -> a -> m b) -> t a -> m (t b)

-- | A default implementation of `traverseWithIndex` using `sequence` and `mapWithIndex`.
traverseWithIndexDefault
  :: forall i t a b m
   . TraversableWithIndex i t
  => Applicative m
  => (i -> a -> m b)
  -> t a
  -> m (t b)
traverseWithIndexDefault f = sequence <<< mapWithIndex f

instance traversableWithIndexArray :: TraversableWithIndex Int Array where
  traverseWithIndex = traverseWithIndexDefault

instance traversableWithIndexMaybe :: TraversableWithIndex Unit Maybe where
  traverseWithIndex f = traverse $ f unit

instance traversableWithIndexFirst :: TraversableWithIndex Unit First where
  traverseWithIndex f = traverse $ f unit

instance traversableWithIndexLast :: TraversableWithIndex Unit Last where
  traverseWithIndex f = traverse $ f unit

instance traversableWithIndexAdditive :: TraversableWithIndex Unit Additive where
  traverseWithIndex f = traverse $ f unit

instance traversableWithIndexDual :: TraversableWithIndex Unit Dual where
  traverseWithIndex f = traverse $ f unit

instance traversableWithIndexConj :: TraversableWithIndex Unit Conj where
  traverseWithIndex f = traverse $ f unit

instance traversableWithIndexDisj :: TraversableWithIndex Unit Disj where
  traverseWithIndex f = traverse $ f unit

instance traversableWithIndexMultiplicative :: TraversableWithIndex Unit Multiplicative where
  traverseWithIndex f = traverse $ f unit

instance traversableWithIndexEither :: TraversableWithIndex Unit (Either a) where
  traverseWithIndex _ (Left x)  = pure (Left x)
  traverseWithIndex f (Right x) = Right <$> f unit x

instance traversableWithIndexTuple :: TraversableWithIndex Unit (Tuple a) where
  traverseWithIndex f (Tuple x y) = Tuple x <$> f unit y

instance traversableWithIndexIdentity :: TraversableWithIndex Unit Identity where
  traverseWithIndex f (Identity x) = Identity <$> f unit x

instance traversableWithIndexConst :: TraversableWithIndex Void (Const a) where
  traverseWithIndex _ (Const x) = pure (Const x)

instance traversableWithIndexProduct :: (TraversableWithIndex a f, TraversableWithIndex b g) => TraversableWithIndex (Either a b) (Product f g) where
  traverseWithIndex f (Product (Tuple fa ga)) = lift2 product (traverseWithIndex (f <<< Left) fa) (traverseWithIndex (f <<< Right) ga)

instance traversableWithIndexCoproduct :: (TraversableWithIndex a f, TraversableWithIndex b g) => TraversableWithIndex (Either a b) (Coproduct f g) where
  traverseWithIndex f = coproduct
    (map (Coproduct <<< Left) <<< traverseWithIndex (f <<< Left))
    (map (Coproduct <<< Right) <<< traverseWithIndex (f <<< Right))

instance traversableWithIndexCompose :: (TraversableWithIndex a f, TraversableWithIndex b g) => TraversableWithIndex (Tuple a b) (Compose f g) where
  traverseWithIndex f (Compose fga) = map Compose $ traverseWithIndex (traverseWithIndex <<< curry f) fga

instance traversableWithIndexApp :: TraversableWithIndex a f => TraversableWithIndex a (App f) where
  traverseWithIndex f (App x) = App <$> traverseWithIndex f x

-- | A version of `traverseWithIndex` with its arguments flipped.
-- |
-- |
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure:
-- |
-- | For example:
-- |
-- | ```purescript
-- | for [1, 2, 3] \i x -> do
-- |   logShow i
-- |   pure (x * x)
-- | ```
forWithIndex
  :: forall i a b m t
   . Applicative m
  => TraversableWithIndex i t
  => t a
  -> (i -> a -> m b)
  -> m (t b)
forWithIndex = flip traverseWithIndex

-- | Fold a data structure from the left with access to the indices, keeping
-- | all intermediate results instead of only the final result. Note that the
-- | initial value does not appear in the result (unlike Haskell's
-- | `Prelude.scanl`).
-- |
-- | ```purescript
-- | scanlWithIndex (\i y x -> i + y + x) 0 [1, 2, 3] = [1, 4, 9]
-- | ```
scanlWithIndex
  :: forall i a b f
   . TraversableWithIndex i f
  => (i -> b -> a -> b)
  -> b
  -> f a
  -> f b
scanlWithIndex f b0 xs =
  (mapAccumLWithIndex (\i b a -> let b' = f i b a in { accum: b', value: b' }) b0 xs).value

-- | Fold a data structure from the left with access to the indices, keeping
-- | all intermediate results instead of only the final result.
-- |
-- | Unlike `scanlWithIndex`, `mapAccumLWithIndex` allows the type of accumulator to differ
-- | from the element type of the final data structure.
mapAccumLWithIndex
  :: forall i a b s f
   . TraversableWithIndex i f
  => (i -> s -> a -> Accum s b)
  -> s
  -> f a
  -> Accum s (f b)
mapAccumLWithIndex f s0 xs = stateL (traverseWithIndex (\i a -> StateL \s -> f i s a) xs) s0

-- | Fold a data structure from the right with access to the indices, keeping
-- | all intermediate results instead of only the final result. Note that the
-- | initial value does not appear in the result (unlike Haskell's `Prelude.scanr`).
-- |
-- | ```purescript
-- | scanrWithIndex (\i x y -> i + x + y) 0 [1, 2, 3] = [9, 8, 5]
-- | ```
scanrWithIndex
  :: forall i a b f
   . TraversableWithIndex i f
  => (i -> a -> b -> b)
  -> b
  -> f a
  -> f b
scanrWithIndex f b0 xs =
  (mapAccumRWithIndex (\i b a -> let b' = f i a b in { accum: b', value: b' }) b0 xs).value

-- | Fold a data structure from the right with access to the indices, keeping
-- | all intermediate results instead of only the final result.
-- |
-- | Unlike `scanrWithIndex`, `imapAccumRWithIndex` allows the type of accumulator to differ
-- | from the element type of the final data structure.
mapAccumRWithIndex
  :: forall i a b s f
   . TraversableWithIndex i f
  => (i -> s -> a -> Accum s b)
  -> s
  -> f a
  -> Accum s (f b)
mapAccumRWithIndex f s0 xs = stateR (traverseWithIndex (\i a -> StateR \s -> f i s a) xs) s0

-- | A default implementation of `traverse` in terms of `traverseWithIndex`
traverseDefault
  :: forall i t a b m
   . TraversableWithIndex i t
  => Applicative m
  => (a -> m b) -> t a -> m (t b)
traverseDefault f = traverseWithIndex (const f)
