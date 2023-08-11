-- | This module defines a type of sets as balanced 2-3 trees, based on
-- | <http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>
-- |
-- | Qualified import is encouraged, so as to avoid name clashes with other modules.

module Data.Set
  ( Set
  , fromFoldable
  , toUnfoldable
  , empty
  , isEmpty
  , singleton
  , map
  , checkValid
  , insert
  , member
  , delete
  , toggle
  , size
  , findMin
  , findMax
  , union
  , unions
  , difference
  , subset
  , properSubset
  , intersection
  , filter
  , mapMaybe
  , catMaybes
  , toMap
  , fromMap
  ) where

import Prelude hiding (map)

import Control.Monad.Rec.Class (Step(..), tailRecM2)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List (List)
import Data.List as List
import Data.Map.Internal as M
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord1)
import Data.Unfoldable (class Unfoldable)
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude

-- | `Set a` represents a set of values of type `a`
newtype Set a = Set (M.Map a Unit)

-- | Create a set from a foldable structure.
fromFoldable :: forall f a. Foldable f => Ord a => f a -> Set a
fromFoldable = foldl (\m a -> insert a m) empty

-- | Convert a set to an unfoldable structure.
toUnfoldable :: forall f a. Unfoldable f => Set a -> f a
toUnfoldable = List.toUnfoldable <<< toList

toList :: forall a. Set a -> List a
toList (Set m) = M.keys m

instance eqSet :: Eq a => Eq (Set a) where
  eq (Set m1) (Set m2) = m1 == m2

instance eq1Set :: Eq1 Set where
  eq1 = eq

instance showSet :: Show a => Show (Set a) where
  show s = "(fromFoldable " <> show (toUnfoldable s :: Array a) <> ")"

instance ordSet :: Ord a => Ord (Set a) where
  compare s1 s2 = compare (toList s1) (toList s2)

instance ord1Set :: Ord1 Set where
  compare1 = compare

instance monoidSet :: Ord a => Monoid (Set a) where
  mempty = empty

instance semigroupSet :: Ord a => Semigroup (Set a) where
  append = union

instance foldableSet :: Foldable Set where
  foldMap f = foldMap f <<< toList
  foldl f x = foldl f x <<< toList
  foldr f x = foldr f x <<< toList

-- | An empty set
empty :: forall a. Set a
empty = Set M.empty

-- | Test if a set is empty
isEmpty :: forall a. Set a -> Boolean
isEmpty (Set m) = M.isEmpty m

-- | Create a set with one element
singleton :: forall a. a -> Set a
singleton a = Set (M.singleton a unit)

-- | Maps over the values in a set.
-- |
-- | This operation is not structure-preserving for sets, so is not a valid
-- | `Functor`. An example case: mapping `const x` over a set with `n > 0`
-- | elements will result in a set with one element.
map :: forall a b. Ord b => (a -> b) -> Set a -> Set b
map f = foldl (\m a -> insert (f a) m) empty

-- | Check whether the underlying tree satisfies the 2-3 invariant
-- |
-- | This function is provided for internal use.
checkValid :: forall a. Set a -> Boolean
checkValid (Set m) = M.checkValid m

-- | Test if a value is a member of a set
member :: forall a. Ord a => a -> Set a -> Boolean
member a (Set m) = a `M.member` m

-- | Insert a value into a set
insert :: forall a. Ord a => a -> Set a -> Set a
insert a (Set m) = Set (M.insert a unit m)

-- | Delete a value from a set
delete :: forall a. Ord a => a -> Set a -> Set a
delete a (Set m) = Set (a `M.delete` m)

-- | Insert a value into a set if it is not already present, if it is present, delete it.
toggle :: forall a. Ord a => a -> Set a -> Set a
toggle a (Set m) = Set (M.alter (maybe (Just unit) (\_ -> Nothing)) a m)

-- | Find the size of a set
size :: forall a. Set a -> Int
size (Set m) = M.size m

findMin :: forall a. Set a -> Maybe a
findMin (Set m) = Prelude.map _.key (M.findMin m)

findMax :: forall a. Set a -> Maybe a
findMax (Set m) = Prelude.map _.key (M.findMax m)

-- | Form the union of two sets
-- |
-- | Running time: `O(n * log(m))`
union :: forall a. Ord a => Set a -> Set a -> Set a
union (Set m1) (Set m2) = Set (m1 `M.union` m2)

-- | Form the union of a collection of sets
unions :: forall f a. Foldable f => Ord a => f (Set a) -> Set a
unions = foldl union empty

-- | Form the set difference
difference :: forall a. Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldl (flip delete) s1 (toList s2)

-- | True if and only if every element in the first set
-- | is an element of the second set
subset :: forall a. Ord a => Set a -> Set a -> Boolean
subset s1 s2 = isEmpty $ s1 `difference` s2

-- | True if and only if the first set is a subset of the second set
-- | and the sets are not equal
properSubset :: forall a. Ord a => Set a -> Set a -> Boolean
properSubset s1 s2 = subset s1 s2 && (s1 /= s2)

-- | The set of elements which are in both the first and second set
intersection :: forall a. Ord a => Set a -> Set a -> Set a
intersection s1 s2 = fromFoldable (ST.run (STArray.new >>= intersect >>= STArray.unsafeFreeze))
  where
  toArray = Array.fromFoldable <<< toList
  ls = toArray s1
  rs = toArray s2
  ll = Array.length ls
  rl = Array.length rs
  intersect :: forall r. STArray r a -> ST r (STArray r a)
  intersect acc = tailRecM2 go 0 0
    where
    go = unsafePartial \l r ->
      if l < ll && r < rl
      then case compare (ls `Array.unsafeIndex` l) (rs `Array.unsafeIndex` r) of
        EQ -> do
          _ <- STArray.push (ls `Array.unsafeIndex` l) acc
          pure $ Loop {a: l + 1, b: r + 1}
        LT -> pure $ Loop {a: l + 1, b: r}
        GT -> pure $ Loop {a: l, b: r + 1}
      else pure $ Done acc

-- | Filter out those values of a set for which a predicate on the value fails
-- | to hold.
filter :: forall a. Ord a => (a -> Boolean) -> Set a -> Set a
filter f (Set s) = Set (M.filterWithKey (\k _ -> f k) s)

-- | Applies a function to each value in a set, discarding entries where the
-- | function returns `Nothing`.
mapMaybe :: forall a b. Ord b => (a -> Maybe b) -> Set a -> Set b
mapMaybe f = foldr (\a acc -> maybe acc (\b -> insert b acc) (f a)) empty

-- | Filter a set of optional values, discarding values that contain `Nothing`
catMaybes :: forall a. Ord a => Set (Maybe a) -> Set a
catMaybes = mapMaybe identity

-- | A set is a map with no value attached to each key.
toMap :: forall a. Set a -> M.Map a Unit
toMap (Set s) = s

-- | A map with no value attached to each key is a set.
-- | See also `Data.Map.keys`.
fromMap :: forall a. M.Map a Unit -> Set a
fromMap = Set
