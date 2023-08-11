module Data.Map
  ( module Data.Map.Internal
  , keys
  , SemigroupMap(..)
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Map.Internal (Map, alter, catMaybes, checkValid, delete, empty, filter, filterKeys, filterWithKey, findMax, findMin, foldSubmap, fromFoldable, fromFoldableWith, fromFoldableWithIndex, insert, insertWith, isEmpty, isSubmap, lookup, lookupGE, lookupGT, lookupLE, lookupLT, member, pop, showTree, singleton, size, submap, toUnfoldable, toUnfoldableUnordered, union, unionWith, unions, intersection, intersectionWith, difference, update, values, mapMaybeWithKey, mapMaybe)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Set (Set, fromMap)

-- | The set of keys of the given map.
-- | See also `Data.Set.fromMap`.
keys :: forall k v. Map k v -> Set k
keys = fromMap <<< void

-- | `SemigroupMap k v` provides a `Semigroup` instance for `Map k v` whose
-- | definition depends on the `Semigroup` instance for the `v` type.
-- | You should only use this type when you need `Data.Map` to have
-- | a `Semigroup` instance.
-- |
-- | ```purescript
-- | let
-- |   s :: forall key value. key -> value -> SemigroupMap key value
-- |   s k v = SemigroupMap (singleton k v)
-- |
-- | (s 1     "foo") <> (s 1     "bar") == (s 1  "foobar")
-- | (s 1 (First 1)) <> (s 1 (First 2)) == (s 1 (First 1))
-- | (s 1  (Last 1)) <> (s 1  (Last 2)) == (s 1  (Last 2))
-- | ```
newtype SemigroupMap k v = SemigroupMap (Map k v)

derive newtype instance eq1SemigroupMap :: Eq k => Eq1 (SemigroupMap k)
derive newtype instance eqSemigroupMap :: (Eq k, Eq v) => Eq (SemigroupMap k v)
derive newtype instance ord1SemigroupMap :: Ord k => Ord1 (SemigroupMap k)
derive newtype instance ordSemigroupMap :: (Ord k, Ord v) => Ord (SemigroupMap k v)
derive instance newtypeSemigroupMap :: Newtype (SemigroupMap k v) _
derive newtype instance showSemigroupMap :: (Show k, Show v) => Show (SemigroupMap k v)

instance semigroupSemigroupMap :: (Ord k, Semigroup v) => Semigroup (SemigroupMap k v) where
  append (SemigroupMap l) (SemigroupMap r) = SemigroupMap (unionWith append l r)

instance monoidSemigroupMap :: (Ord k, Semigroup v) => Monoid (SemigroupMap k v) where
  mempty = SemigroupMap empty

derive newtype instance altSemigroupMap :: Ord k => Alt (SemigroupMap k)
derive newtype instance plusSemigroupMap :: Ord k => Plus (SemigroupMap k)
derive newtype instance functorSemigroupMap :: Functor (SemigroupMap k)
derive newtype instance functorWithIndexSemigroupMap :: FunctorWithIndex k (SemigroupMap k)
derive newtype instance applySemigroupMap :: Ord k => Apply (SemigroupMap k)
derive newtype instance bindSemigroupMap :: Ord k => Bind (SemigroupMap k)
derive newtype instance foldableSemigroupMap :: Foldable (SemigroupMap k)
derive newtype instance foldableWithIndexSemigroupMap :: FoldableWithIndex k (SemigroupMap k)
derive newtype instance traversableSemigroupMap :: Traversable (SemigroupMap k)
derive newtype instance traversableWithIndexSemigroupMap :: TraversableWithIndex k (SemigroupMap k)
