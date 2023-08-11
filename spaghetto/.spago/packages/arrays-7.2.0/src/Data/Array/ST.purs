-- | Helper functions for working with mutable arrays using the `ST` effect.
-- |
-- | This module can be used when performance is important and mutation is a local effect.

module Data.Array.ST
  ( STArray(..)
  , Assoc
  , run
  , withArray
  , new
  , peek
  , poke
  , modify
  , length
  , pop
  , push
  , pushAll
  , shift
  , unshift
  , unshiftAll
  , splice
  , sort
  , sortBy
  , sortWith
  , freeze
  , thaw
  , unsafeFreeze
  , unsafeThaw
  , toAssocArray
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST (ST, Region)
import Data.Maybe (Maybe(..))

-- | A reference to a mutable array.
-- |
-- | The first type parameter represents the memory region which the array belongs to.
-- | The second type parameter defines the type of elements of the mutable array.
-- |
-- | The runtime representation of a value of type `STArray h a` is the same as that of `Array a`,
-- | except that mutation is allowed.
foreign import data STArray :: Region -> Type -> Type

type role STArray nominal representational

-- | An element and its index.
type Assoc a = { value :: a, index :: Int }

-- | A safe way to create and work with a mutable array before returning an
-- | immutable array for later perusal. This function avoids copying the array
-- | before returning it - it uses unsafeFreeze internally, but this wrapper is
-- | a safe interface to that function.
run :: forall a. (forall h. ST h (STArray h a)) -> Array a
run st = ST.run (st >>= unsafeFreeze)

-- | Perform an effect requiring a mutable array on a copy of an immutable array,
-- | safely returning the result as an immutable array.
withArray
  :: forall h a b
   . (STArray h a -> ST h b)
   -> Array a
   -> ST h (Array a)
withArray f xs = do
  result <- thaw xs
  _ <- f result
  unsafeFreeze result

-- | O(1). Convert a mutable array to an immutable array, without copying. The mutable
-- | array must not be mutated afterwards.
foreign import unsafeFreeze :: forall h a. STArray h a -> ST h (Array a)

-- | O(1) Convert an immutable array to a mutable array, without copying. The input
-- | array must not be used afterward.
foreign import unsafeThaw :: forall h a. Array a -> ST h (STArray h a)

-- | Create a new, empty mutable array.
foreign import new :: forall h a. ST h (STArray h a)

-- | Create a mutable copy of an immutable array.
foreign import thaw :: forall h a. Array a -> ST h (STArray h a)

-- | Sort a mutable array in place. Sorting is stable: the order of equal
-- | elements is preserved.
sort :: forall a h. Ord a => STArray h a -> ST h (STArray h a)
sort = sortBy compare

-- | Remove the first element from an array and return that element.
shift :: forall h a. STArray h a -> ST h (Maybe a)
shift = shiftImpl Just Nothing

foreign import shiftImpl
  :: forall h a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> STArray h a
  -> ST h (Maybe a)

-- | Sort a mutable array in place using a comparison function. Sorting is
-- | stable: the order of elements is preserved if they are equal according to
-- | the comparison function.
sortBy
  :: forall a h
   . (a -> a -> Ordering)
  -> STArray h a
  -> ST h (STArray h a)
sortBy comp = sortByImpl comp case _ of
  GT -> 1
  EQ -> 0
  LT -> -1

foreign import sortByImpl
  :: forall a h
   . (a -> a -> Ordering)
  -> (Ordering -> Int)
  -> STArray h a
  -> ST h (STArray h a)

-- | Sort a mutable array in place based on a projection. Sorting is stable: the
-- | order of elements is preserved if they are equal according to the projection.
sortWith
  :: forall a b h
   . Ord b
  => (a -> b)
  -> STArray h a
  -> ST h (STArray h a)
sortWith f = sortBy (comparing f)

-- | Create an immutable copy of a mutable array.
foreign import freeze :: forall h a. STArray h a -> ST h (Array a)

-- | Read the value at the specified index in a mutable array.
peek
  :: forall h a
   . Int
  -> STArray h a
  -> ST h (Maybe a)
peek = peekImpl Just Nothing

foreign import peekImpl
  :: forall h a r
   . (a -> r)
  -> r
  -> Int
  -> STArray h a
  -> (ST h r)

-- | Change the value at the specified index in a mutable array.
foreign import poke :: forall h a. Int -> a -> STArray h a -> ST h Boolean

-- | Get the number of elements in a mutable array.
foreign import length :: forall h a. STArray h a -> ST h Int

-- | Remove the last element from an array and return that element.
pop :: forall h a. STArray h a -> ST h (Maybe a)
pop = popImpl Just Nothing

foreign import popImpl
  :: forall h a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> STArray h a
  -> ST h (Maybe a)

-- | Append an element to the end of a mutable array. Returns the new length of
-- | the array.
push :: forall h a. a -> STArray h a -> ST h Int
push a = pushAll [a]

-- | Append the values in an immutable array to the end of a mutable array.
-- | Returns the new length of the mutable array.
foreign import pushAll
  :: forall h a
   . Array a
  -> STArray h a
  -> ST h Int

-- | Append an element to the front of a mutable array. Returns the new length of
-- | the array.
unshift :: forall h a. a -> STArray h a -> ST h Int
unshift a = unshiftAll [a]

-- | Append the values in an immutable array to the front of a mutable array.
-- | Returns the new length of the mutable array.
foreign import unshiftAll
  :: forall h a
   . Array a
  -> STArray h a
  -> ST h Int

-- | Mutate the element at the specified index using the supplied function.
modify :: forall h a. Int -> (a -> a) -> STArray h a -> ST h Boolean
modify i f xs = do
  entry <- peek i xs
  case entry of
    Just x  -> poke i (f x) xs
    Nothing -> pure false

-- | Remove and/or insert elements from/into a mutable array at the specified index.
foreign import splice
  :: forall h a
   . Int
  -> Int
  -> Array a
  -> STArray h a
  -> ST h (Array a)

-- | Create an immutable copy of a mutable array, where each element
-- | is labelled with its index in the original array.
foreign import toAssocArray :: forall h a. STArray h a -> ST h (Array (Assoc a))
