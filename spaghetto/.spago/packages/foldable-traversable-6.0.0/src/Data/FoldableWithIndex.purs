module Data.FoldableWithIndex
  ( class FoldableWithIndex, foldrWithIndex, foldlWithIndex, foldMapWithIndex
  , foldrWithIndexDefault
  , foldlWithIndexDefault
  , foldMapWithIndexDefaultR
  , foldMapWithIndexDefaultL
  , foldWithIndexM
  , traverseWithIndex_
  , forWithIndex_
  , surroundMapWithIndex
  , allWithIndex
  , anyWithIndex
  , findWithIndex
  , findMapWithIndex
  , foldrDefault
  , foldlDefault
  , foldMapDefault
  ) where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Functor.App (App(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.Functor.Product (Product(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), curry)

-- | A `Foldable` with an additional index.
-- | A `FoldableWithIndex` instance must be compatible with its `Foldable`
-- | instance
-- | ```purescript
-- | foldr f = foldrWithIndex (const f)
-- | foldl f = foldlWithIndex (const f)
-- | foldMap f = foldMapWithIndex (const f)
-- | ```
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `foldrWithIndexDefault`
-- | - `foldlWithIndexDefault`
-- | - `foldMapWithIndexDefaultR`
-- | - `foldMapWithIndexDefaultL`
-- |
-- | Note: some combinations of the default implementations are unsafe to
-- | use together - causing a non-terminating mutually recursive cycle.
-- | These combinations are documented per function.
class Foldable f <= FoldableWithIndex i f | f -> i where
  foldrWithIndex :: forall a b. (i -> a -> b -> b) -> b -> f a -> b
  foldlWithIndex :: forall a b. (i -> b -> a -> b) -> b -> f a -> b
  foldMapWithIndex :: forall a m. Monoid m => (i -> a -> m) -> f a -> m

-- | A default implementation of `foldrWithIndex` using `foldMapWithIndex`.
-- |
-- | Note: when defining a `FoldableWithIndex` instance, this function is
-- | unsafe to use in combination with `foldMapWithIndexDefaultR`.
foldrWithIndexDefault
  :: forall i f a b
   . FoldableWithIndex i f
  => (i -> a -> b -> b)
  -> b
  -> f a
  -> b
foldrWithIndexDefault c u xs = unwrap (foldMapWithIndex (\i -> Endo <<< c i) xs) u

-- | A default implementation of `foldlWithIndex` using `foldMapWithIndex`.
-- |
-- | Note: when defining a `FoldableWithIndex` instance, this function is
-- | unsafe to use in combination with `foldMapWithIndexDefaultL`.
foldlWithIndexDefault
  :: forall i f a b
   . FoldableWithIndex i f
  => (i -> b -> a -> b)
  -> b
  -> f a
  -> b
foldlWithIndexDefault c u xs = unwrap (unwrap (foldMapWithIndex (\i -> Dual <<< Endo <<< flip (c i)) xs)) u

-- | A default implementation of `foldMapWithIndex` using `foldrWithIndex`.
-- |
-- | Note: when defining a `FoldableWithIndex` instance, this function is
-- | unsafe to use in combination with `foldrWithIndexDefault`.
foldMapWithIndexDefaultR
  :: forall i f a m
   . FoldableWithIndex i f
  => Monoid m
  => (i -> a -> m)
  -> f a
  -> m
foldMapWithIndexDefaultR f = foldrWithIndex (\i x acc -> f i x <> acc) mempty

-- | A default implementation of `foldMapWithIndex` using `foldlWithIndex`.
-- |
-- | Note: when defining a `FoldableWithIndex` instance, this function is
-- | unsafe to use in combination with `foldlWithIndexDefault`.
foldMapWithIndexDefaultL
  :: forall i f a m
   . FoldableWithIndex i f
  => Monoid m
  => (i -> a -> m)
  -> f a
  -> m
foldMapWithIndexDefaultL f = foldlWithIndex (\i acc x -> acc <> f i x) mempty

instance foldableWithIndexArray :: FoldableWithIndex Int Array where
  foldrWithIndex f z = foldr (\(Tuple i x) y -> f i x y) z <<< mapWithIndex Tuple
  foldlWithIndex f z = foldl (\y (Tuple i x) -> f i y x) z <<< mapWithIndex Tuple
  foldMapWithIndex = foldMapWithIndexDefaultR

instance foldableWithIndexMaybe :: FoldableWithIndex Unit Maybe where
  foldrWithIndex f = foldr $ f unit
  foldlWithIndex f = foldl $ f unit
  foldMapWithIndex f = foldMap $ f unit

instance foldableWithIndexFirst :: FoldableWithIndex Unit First where
  foldrWithIndex f = foldr $ f unit
  foldlWithIndex f = foldl $ f unit
  foldMapWithIndex f = foldMap $ f unit

instance foldableWithIndexLast :: FoldableWithIndex Unit Last where
  foldrWithIndex f = foldr $ f unit
  foldlWithIndex f = foldl $ f unit
  foldMapWithIndex f = foldMap $ f unit

instance foldableWithIndexAdditive :: FoldableWithIndex Unit Additive where
  foldrWithIndex f = foldr $ f unit
  foldlWithIndex f = foldl $ f unit
  foldMapWithIndex f = foldMap $ f unit

instance foldableWithIndexDual :: FoldableWithIndex Unit Dual where
  foldrWithIndex f = foldr $ f unit
  foldlWithIndex f = foldl $ f unit
  foldMapWithIndex f = foldMap $ f unit

instance foldableWithIndexDisj :: FoldableWithIndex Unit Disj where
  foldrWithIndex f = foldr $ f unit
  foldlWithIndex f = foldl $ f unit
  foldMapWithIndex f = foldMap $ f unit

instance foldableWithIndexConj :: FoldableWithIndex Unit Conj where
  foldrWithIndex f = foldr $ f unit
  foldlWithIndex f = foldl $ f unit
  foldMapWithIndex f = foldMap $ f unit

instance foldableWithIndexMultiplicative :: FoldableWithIndex Unit Multiplicative where
  foldrWithIndex f = foldr $ f unit
  foldlWithIndex f = foldl $ f unit
  foldMapWithIndex f = foldMap $ f unit

instance foldableWithIndexEither :: FoldableWithIndex Unit (Either a) where
  foldrWithIndex _ z (Left _)  = z
  foldrWithIndex f z (Right x) = f unit x z
  foldlWithIndex _ z (Left _)  = z
  foldlWithIndex f z (Right x) = f unit z x
  foldMapWithIndex _ (Left _)  = mempty
  foldMapWithIndex f (Right x) = f unit x

instance foldableWithIndexTuple :: FoldableWithIndex Unit (Tuple a) where
  foldrWithIndex f z (Tuple _ x) = f unit x z
  foldlWithIndex f z (Tuple _ x) = f unit z x
  foldMapWithIndex f (Tuple _ x) = f unit x

instance foldableWithIndexIdentity :: FoldableWithIndex Unit Identity where
  foldrWithIndex f z (Identity x) = f unit x z
  foldlWithIndex f z (Identity x) = f unit z x
  foldMapWithIndex f (Identity x) = f unit x

instance foldableWithIndexConst :: FoldableWithIndex Void (Const a) where
  foldrWithIndex _ z _ = z
  foldlWithIndex _ z _ = z
  foldMapWithIndex _ _ = mempty

instance foldableWithIndexProduct :: (FoldableWithIndex a f, FoldableWithIndex b g) => FoldableWithIndex (Either a b) (Product f g) where
  foldrWithIndex f z (Product (Tuple fa ga)) = foldrWithIndex (f <<< Left) (foldrWithIndex (f <<< Right) z ga) fa
  foldlWithIndex f z (Product (Tuple fa ga)) = foldlWithIndex (f <<< Right) (foldlWithIndex (f <<< Left) z fa) ga
  foldMapWithIndex f (Product (Tuple fa ga)) = foldMapWithIndex (f <<< Left) fa <> foldMapWithIndex (f <<< Right) ga

instance foldableWithIndexCoproduct :: (FoldableWithIndex a f, FoldableWithIndex b g) => FoldableWithIndex (Either a b) (Coproduct f g) where
  foldrWithIndex f z = coproduct (foldrWithIndex (f <<< Left) z) (foldrWithIndex (f <<< Right) z)
  foldlWithIndex f z = coproduct (foldlWithIndex (f <<< Left) z) (foldlWithIndex (f <<< Right) z)
  foldMapWithIndex f = coproduct (foldMapWithIndex (f <<< Left)) (foldMapWithIndex (f <<< Right))

instance foldableWithIndexCompose :: (FoldableWithIndex a f, FoldableWithIndex b g) => FoldableWithIndex (Tuple a b) (Compose f g) where
  foldrWithIndex f i (Compose fga) = foldrWithIndex (\a -> flip (foldrWithIndex (curry f a))) i fga
  foldlWithIndex f i (Compose fga) = foldlWithIndex (foldlWithIndex <<< curry f) i fga
  foldMapWithIndex f (Compose fga) = foldMapWithIndex (foldMapWithIndex <<< curry f) fga

instance foldableWithIndexApp :: FoldableWithIndex a f => FoldableWithIndex a (App f) where
  foldrWithIndex f z (App x) = foldrWithIndex f z x
  foldlWithIndex f z (App x) = foldlWithIndex f z x
  foldMapWithIndex f (App x) = foldMapWithIndex f x


-- | Similar to 'foldlWithIndex', but the result is encapsulated in a monad.
-- |
-- | Note: this function is not generally stack-safe, e.g., for monads which
-- | build up thunks a la `Eff`.
foldWithIndexM
  :: forall i f m a b
   . FoldableWithIndex i f
  => Monad m
  => (i -> a -> b -> m a)
  -> a
  -> f b
  -> m a
foldWithIndexM f a0 = foldlWithIndex (\i ma b -> ma >>= flip (f i) b) (pure a0)

-- | Traverse a data structure with access to the index, performing some
-- | effects encoded by an `Applicative` functor at each value, ignoring the
-- | final result.
-- |
-- | For example:
-- |
-- | ```purescript
-- | > traverseWithIndex_ (curry logShow) ["a", "b", "c"]
-- | (Tuple 0 "a")
-- | (Tuple 1 "b")
-- | (Tuple 2 "c")
-- | ```
traverseWithIndex_
  :: forall i a b f m
   . Applicative m
  => FoldableWithIndex i f
  => (i -> a -> m b)
  -> f a
  -> m Unit
traverseWithIndex_ f = foldrWithIndex (\i -> (*>) <<< f i) (pure unit)

-- | A version of `traverseWithIndex_` with its arguments flipped.
-- |
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure:
-- |
-- | For example:
-- |
-- | ```purescript
-- | forWithIndex_ ["a", "b", "c"] \i x -> do
-- |   logShow i
-- |   log x
-- | ```
forWithIndex_
  :: forall i a b f m
   . Applicative m
  => FoldableWithIndex i f
  => f a
  -> (i -> a -> m b)
  -> m Unit
forWithIndex_ = flip traverseWithIndex_

-- | `foldMapWithIndex` but with each element surrounded by some fixed value.
-- |
-- | For example:
-- |
-- | ```purescript
-- | > surroundMapWithIndex "*" (\i x -> show i <> x) []
-- | = "*"
-- |
-- | > surroundMapWithIndex "*" (\i x -> show i <> x) ["a"]
-- | = "*0a*"
-- |
-- | > surroundMapWithIndex "*" (\i x -> show i <> x) ["a", "b"]
-- | = "*0a*1b*"
-- |
-- | > surroundMapWithIndex "*" (\i x -> show i <> x) ["a", "b", "c"]
-- | = "*0a*1b*2c*"
-- | ```
surroundMapWithIndex
  :: forall i f a m
   . FoldableWithIndex i f
  => Semigroup m
  => m
  -> (i -> a -> m)
  -> f a
  -> m
surroundMapWithIndex d t f = unwrap (foldMapWithIndex joined f) d
  where joined i a = Endo \m -> d <> t i a <> m

-- | `allWithIndex f` is the same as `and <<< mapWithIndex f`; map a function over the
-- | structure, and then get the conjunction of the results.
allWithIndex
  :: forall i a b f
   . FoldableWithIndex i f
  => HeytingAlgebra b
  => (i -> a -> b)
  -> f a
  -> b
allWithIndex t = unwrap <<< foldMapWithIndex (\i -> Conj <<< t i)

-- | `anyWithIndex f` is the same as `or <<< mapWithIndex f`; map a function over the
-- | structure, and then get the disjunction of the results.
anyWithIndex
  :: forall i a b f
   . FoldableWithIndex i f
  => HeytingAlgebra b
  => (i -> a -> b)
  -> f a
  -> b
anyWithIndex t = unwrap <<< foldMapWithIndex (\i -> Disj <<< t i)

-- | Try to find an element in a data structure which satisfies a predicate
-- | with access to the index.
findWithIndex
  :: forall i a f
   . FoldableWithIndex i f
  => (i -> a -> Boolean)
  -> f a
  -> Maybe { index :: i, value :: a }
findWithIndex p = foldlWithIndex go Nothing
  where
    go
      :: i
      -> Maybe { index :: i, value :: a }
      -> a
      -> Maybe { index :: i, value :: a }
    go i Nothing x | p i x = Just { index: i, value: x }
    go _ r _ = r

-- | Try to find an element in a data structure which satisfies a predicate mapping
-- | with access to the index.
findMapWithIndex
  :: forall i a b f
   . FoldableWithIndex i f
  => (i -> a -> Maybe b)
  -> f a
  -> Maybe b
findMapWithIndex f = foldlWithIndex go Nothing
  where
    go
      :: i
      -> Maybe b
      -> a
      -> Maybe b
    go i Nothing x = f i x
    go _ r _ = r

-- | A default implementation of `foldr` using `foldrWithIndex`
foldrDefault
  :: forall i f a b
   . FoldableWithIndex i f
  => (a -> b -> b) -> b -> f a -> b
foldrDefault f = foldrWithIndex (const f)

-- | A default implementation of `foldl` using `foldlWithIndex`
foldlDefault
  :: forall i f a b
   . FoldableWithIndex i f
  => (b -> a -> b) -> b -> f a -> b
foldlDefault f = foldlWithIndex (const f)

-- | A default implementation of `foldMap` using `foldMapWithIndex`
foldMapDefault
  :: forall i f a m
   . FoldableWithIndex i f
  => Monoid m
  => (a -> m) -> f a -> m
foldMapDefault f = foldMapWithIndex (const f)
