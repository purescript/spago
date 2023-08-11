-- | Helper functions for working with immutable Javascript arrays.
-- |
-- | _Note_: Depending on your use-case, you may prefer to use `Data.List` or
-- | `Data.Sequence` instead, which might give better performance for certain
-- | use cases. This module is useful when integrating with JavaScript libraries
-- | which use arrays, but immutable arrays are not a practical data structure
-- | for many use cases due to their poor asymptotics.
-- |
-- | In addition to the functions in this module, Arrays have a number of
-- | useful instances:
-- |
-- | * `Functor`, which provides `map :: forall a b. (a -> b) -> Array a ->
-- |   Array b`
-- | * `Apply`, which provides `(<*>) :: forall a b. Array (a -> b) -> Array a
-- |   -> Array b`. This function works a bit like a Cartesian product; the
-- |   result array is constructed by applying each function in the first
-- |   array to each value in the second, so that the result array ends up with
-- |   a length equal to the product of the two arguments' lengths.
-- | * `Bind`, which provides `(>>=) :: forall a b. (a -> Array b) -> Array a
-- |   -> Array b` (this is the same as `concatMap`).
-- | * `Semigroup`, which provides `(<>) :: forall a. Array a -> Array a ->
-- |   Array a`, for concatenating arrays.
-- | * `Foldable`, which provides a slew of functions for *folding* (also known
-- |   as *reducing*) arrays down to one value. For example,
-- |   `Data.Foldable.or` tests whether an array of `Boolean` values contains
-- |   at least one `true` value.
-- | * `Traversable`, which provides the PureScript version of a for-loop,
-- |   allowing you to STAI.iterate over an array and accumulate effects.
-- |
module Data.Array
  ( fromFoldable
  , toUnfoldable
  , singleton
  , (..), range
  , replicate
  , some
  , many

  , null
  , length

  , (:), cons
  , snoc
  , insert
  , insertBy

  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc

  , (!!), index
  , elem
  , notElem
  , elemIndex
  , elemLastIndex
  , find
  , findMap
  , findIndex
  , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , updateAtIndices
  , modifyAt
  , modifyAtIndices
  , alterAt

  , intersperse
  , reverse
  , concat
  , concatMap
  , filter
  , partition
  , splitAt
  , filterA
  , mapMaybe
  , catMaybes
  , mapWithIndex
  , foldl
  , foldr
  , foldMap
  , fold
  , intercalate
  , transpose
  , scanl
  , scanr

  , sort
  , sortBy
  , sortWith
  , slice
  , take
  , takeEnd
  , takeWhile
  , drop
  , dropEnd
  , dropWhile
  , span
  , group
  , groupAll
  , groupBy
  , groupAllBy

  , nub
  , nubEq
  , nubBy
  , nubByEq
  , union
  , unionBy
  , delete
  , deleteBy

  , (\\), difference
  , intersect
  , intersectBy

  , zipWith
  , zipWithA
  , zip
  , unzip

  , any
  , all

  , foldM
  , foldRecM

  , unsafeIndex
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy, defer)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Control.Monad.ST as ST
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Array.ST as STA
import Data.Array.ST.Iterator as STAI
import Data.Foldable (class Foldable, traverse_)
import Data.Foldable as F
import Data.FunctorWithIndex as FWI
import Data.Maybe (Maybe(..), maybe, isJust, fromJust, isNothing)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Partial.Unsafe (unsafePartial)

-- | Convert an `Array` into an `Unfoldable` structure.
toUnfoldable :: forall f. Unfoldable f => Array ~> f
toUnfoldable xs = unfoldr f 0
  where
  len = length xs
  f i
    | i < len   = Just (Tuple (unsafePartial (unsafeIndex xs i)) (i+1))
    | otherwise = Nothing

-- | Convert a `Foldable` structure into an `Array`.
-- |
-- | ```purescript
-- | fromFoldable (Just 1) = [1]
-- | fromFoldable (Nothing) = []
-- | ```
-- |
fromFoldable :: forall f. Foldable f => f ~> Array
fromFoldable = fromFoldableImpl F.foldr

foreign import fromFoldableImpl
  :: forall f a
   . (forall b. (a -> b -> b) -> b -> f a -> b)
  -> f a
  -> Array a

-- | Create an array of one element
-- | ```purescript
-- | singleton 2 = [2]
-- | ```
singleton :: forall a. a -> Array a
singleton a = [a]

-- | Create an array containing a range of integers, including both endpoints.
-- | ```purescript
-- | range 2 5 = [2, 3, 4, 5]
-- | ```
foreign import range :: Int -> Int -> Array Int

-- | Create an array containing a value repeated the specified number of times.
-- | ```purescript
-- | replicate 2 "Hi" = ["Hi", "Hi"]
-- | ```
foreign import replicate :: forall a. Int -> a -> Array a

-- | An infix synonym for `range`.
-- | ```purescript
-- | 2 .. 5 = [2, 3, 4, 5]
-- | ```
infix 8 range as ..

-- | Attempt a computation multiple times, requiring at least one success.
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
some :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)
some v = (:) <$> v <*> defer (\_ -> many v)

-- | Attempt a computation multiple times, returning as many successful results
-- | as possible (possibly zero).
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
many :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)
many v = some v <|> pure []

--------------------------------------------------------------------------------
-- Array size ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether an array is empty.
-- | ```purescript
-- | null [] = true
-- | null [1, 2] = false
-- | ```
null :: forall a. Array a -> Boolean
null xs = length xs == 0

-- | Get the number of elements in an array.
-- | ```purescript
-- | length ["Hello", "World"] = 2
-- | ```
foreign import length :: forall a. Array a -> Int

--------------------------------------------------------------------------------
-- Extending arrays ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Attaches an element to the front of an array, creating a new array.
-- |
-- | ```purescript
-- | cons 1 [2, 3, 4] = [1, 2, 3, 4]
-- | ```
-- |
-- | Note, the running time of this function is `O(n)`.
cons :: forall a. a -> Array a -> Array a
cons x xs = [x] <> xs

-- | An infix alias for `cons`.
-- |
-- | ```purescript
-- | 1 : [2, 3, 4] = [1, 2, 3, 4]
-- | ```
-- |
-- | Note, the running time of this function is `O(n)`.
infixr 6 cons as :

-- | Append an element to the end of an array, creating a new array.
-- |
-- | ```purescript
-- | snoc [1, 2, 3] 4 = [1, 2, 3, 4]
-- | ```
-- |
snoc :: forall a. Array a -> a -> Array a
snoc xs x = ST.run (STA.withArray (STA.push x) xs)

-- | Insert an element into a sorted array.
-- |
-- | ```purescript
-- | insert 10 [1, 2, 20, 21] = [1, 2, 10, 20, 21]
-- | ```
-- |
insert :: forall a. Ord a => a -> Array a -> Array a
insert = insertBy compare

-- | Insert an element into a sorted array, using the specified function to
-- | determine the ordering of elements.
-- |
-- | ```purescript
-- | invertCompare a b = invert $ compare a b
-- |
-- | insertBy invertCompare 10 [21, 20, 2, 1] = [21, 20, 10, 2, 1]
-- | ```
-- |
insertBy :: forall a. (a -> a -> Ordering) -> a -> Array a -> Array a
insertBy cmp x ys =
  let i = maybe 0 (_ + 1) (findLastIndex (\y -> cmp x y == GT) ys)
  in unsafePartial (fromJust (insertAt i x ys))

--------------------------------------------------------------------------------
-- Non-indexed reads -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the first element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
-- |
-- | ```purescript
-- | head [1, 2] = Just 1
-- | head [] = Nothing
-- | ```
-- |
head :: forall a. Array a -> Maybe a
head xs = xs !! 0

-- | Get the last element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
-- |
-- | ```purescript
-- | last [1, 2] = Just 2
-- | last [] = Nothing
-- | ```
-- |
last :: forall a. Array a -> Maybe a
last xs = xs !! (length xs - 1)

-- | Get all but the first element of an array, creating a new array, or
-- | `Nothing` if the array is empty
-- |
-- | ```purescript
-- | tail [1, 2, 3, 4] = Just [2, 3, 4]
-- | tail [] = Nothing
-- | ```
-- |
-- | Running time: `O(n)` where `n` is the length of the array
tail :: forall a. Array a -> Maybe (Array a)
tail = unconsImpl (const Nothing) (\_ xs -> Just xs)

-- | Get all but the last element of an array, creating a new array, or
-- | `Nothing` if the array is empty.
-- |
-- | ```purescript
-- | init [1, 2, 3, 4] = Just [1, 2, 3]
-- | init [] = Nothing
-- | ```
-- |
-- | Running time: `O(n)` where `n` is the length of the array
init :: forall a. Array a -> Maybe (Array a)
init xs
  | null xs = Nothing
  | otherwise = Just (slice zero (length xs - one) xs)

-- | Break an array into its first element and remaining elements.
-- |
-- | Using `uncons` provides a way of writing code that would use cons patterns
-- | in Haskell or pre-PureScript 0.7:
-- | ``` purescript
-- | f (x : xs) = something
-- | f [] = somethingElse
-- | ```
-- | Becomes:
-- | ``` purescript
-- | f arr = case uncons arr of
-- |   Just { head: x, tail: xs } -> something
-- |   Nothing -> somethingElse
-- | ```
uncons :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
uncons = unconsImpl (const Nothing) \x xs -> Just { head: x, tail: xs }

foreign import unconsImpl
  :: forall a b
   . (Unit -> b)
  -> (a -> Array a -> b)
  -> Array a
  -> b

-- | Break an array into its last element and all preceding elements.
-- |
-- | ```purescript
-- | unsnoc [1, 2, 3] = Just {init: [1, 2], last: 3}
-- | unsnoc [] = Nothing
-- | ```
-- |
-- | Running time: `O(n)` where `n` is the length of the array
unsnoc :: forall a. Array a -> Maybe { init :: Array a, last :: a }
unsnoc xs = { init: _, last: _ } <$> init xs <*> last xs

--------------------------------------------------------------------------------
-- Indexed operations ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | This function provides a safe way to read a value at a particular index
-- | from an array.
-- |
-- | ```purescript
-- | sentence = ["Hello", "World", "!"]
-- |
-- | index sentence 0 = Just "Hello"
-- | index sentence 7 = Nothing
-- | ```
-- |
index :: forall a. Array a -> Int -> Maybe a
index = indexImpl Just Nothing

foreign import indexImpl
  :: forall a
   . (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> Array a
  -> Int
  -> Maybe a

-- | An infix version of `index`.
-- |
-- | ```purescript
-- | sentence = ["Hello", "World", "!"]
-- |
-- | sentence !! 0 = Just "Hello"
-- | sentence !! 7 = Nothing
-- | ```
-- |
infixl 8 index as !!

-- | Returns true if the array has the given element.
elem :: forall a. Eq a => a -> Array a -> Boolean
elem a arr = isJust $ elemIndex a arr

-- | Returns true if the array does not have the given element.
notElem :: forall a. Eq a => a -> Array a -> Boolean
notElem a arr = isNothing $ elemIndex a arr

-- | Find the index of the first element equal to the specified element.
-- |
-- | ```purescript
-- | elemIndex "a" ["a", "b", "a", "c"] = Just 0
-- | elemIndex "Earth" ["Hello", "World", "!"] = Nothing
-- | ```
-- |
elemIndex :: forall a. Eq a => a -> Array a -> Maybe Int
elemIndex x = findIndex (_ == x)

-- | Find the index of the last element equal to the specified element.
-- |
-- | ```purescript
-- | elemLastIndex "a" ["a", "b", "a", "c"] = Just 2
-- | elemLastIndex "Earth" ["Hello", "World", "!"] = Nothing
-- | ```
-- |
elemLastIndex :: forall a. Eq a => a -> Array a -> Maybe Int
elemLastIndex x = findLastIndex (_ == x)

-- | Find the first element for which a predicate holds.
-- |
-- | ```purescript
-- | find (contains $ Pattern "b") ["a", "bb", "b", "d"] = Just "bb"
-- | find (contains $ Pattern "x") ["a", "bb", "b", "d"] = Nothing
-- | ```
find :: forall a. (a -> Boolean) -> Array a -> Maybe a
find f xs = unsafePartial (unsafeIndex xs) <$> findIndex f xs

-- | Find the first element in a data structure which satisfies
-- | a predicate mapping.
findMap :: forall a b. (a -> Maybe b) -> Array a -> Maybe b
findMap = findMapImpl Nothing isJust

foreign import findMapImpl
  :: forall a b
   . (forall c. Maybe c)
  -> (forall c. Maybe c -> Boolean)
  -> (a -> Maybe b)
  -> Array a
  -> Maybe b

-- | Find the first index for which a predicate holds.
-- |
-- | ```purescript
-- | findIndex (contains $ Pattern "b") ["a", "bb", "b", "d"] = Just 1
-- | findIndex (contains $ Pattern "x") ["a", "bb", "b", "d"] = Nothing
-- | ```
-- |
findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex = findIndexImpl Just Nothing

foreign import findIndexImpl
  :: forall a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> (a -> Boolean)
  -> Array a
  -> Maybe Int

-- | Find the last index for which a predicate holds.
-- |
-- | ```purescript
-- | findLastIndex (contains $ Pattern "b") ["a", "bb", "b", "d"] = Just 2
-- | findLastIndex (contains $ Pattern "x") ["a", "bb", "b", "d"] = Nothing
-- | ```
-- |
findLastIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findLastIndex = findLastIndexImpl Just Nothing

foreign import findLastIndexImpl
  :: forall a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> (a -> Boolean)
  -> Array a
  -> Maybe Int

-- | Insert an element at the specified index, creating a new array, or
-- | returning `Nothing` if the index is out of bounds.
-- |
-- | ```purescript
-- | insertAt 2 "!" ["Hello", "World"] = Just ["Hello", "World", "!"]
-- | insertAt 10 "!" ["Hello"] = Nothing
-- | ```
-- |
insertAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
insertAt = _insertAt Just Nothing

foreign import _insertAt
  :: forall a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> Int
  -> a
  -> Array a
  -> Maybe (Array a)

-- | Delete the element at the specified index, creating a new array, or
-- | returning `Nothing` if the index is out of bounds.
-- |
-- | ```purescript
-- | deleteAt 0 ["Hello", "World"] = Just ["World"]
-- | deleteAt 10 ["Hello", "World"] = Nothing
-- | ```
-- |
deleteAt :: forall a. Int -> Array a -> Maybe (Array a)
deleteAt = _deleteAt Just Nothing

foreign import _deleteAt
  :: forall a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> Int
  -> Array a
  -> Maybe (Array a)

-- | Change the element at the specified index, creating a new array, or
-- | returning `Nothing` if the index is out of bounds.
-- |
-- | ```purescript
-- | updateAt 1 "World" ["Hello", "Earth"] = Just ["Hello", "World"]
-- | updateAt 10 "World" ["Hello", "Earth"] = Nothing
-- | ```
-- |
updateAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
updateAt = _updateAt Just Nothing

foreign import _updateAt
  :: forall a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> Int
  -> a
  -> Array a
  -> Maybe (Array a)

-- | Apply a function to the element at the specified index, creating a new
-- | array, or returning `Nothing` if the index is out of bounds.
-- |
-- | ```purescript
-- | modifyAt 1 toUpper ["Hello", "World"] = Just ["Hello", "WORLD"]
-- | modifyAt 10 toUpper ["Hello", "World"] = Nothing
-- | ```
-- |
modifyAt :: forall a. Int -> (a -> a) -> Array a -> Maybe (Array a)
modifyAt i f xs = maybe Nothing go (xs !! i)
  where
  go x = updateAt i (f x) xs

-- | Update or delete the element at the specified index by applying a
-- | function to the current value, returning a new array or `Nothing` if the
-- | index is out-of-bounds.
-- |
-- | ```purescript
-- | alterAt 1 (stripSuffix $ Pattern "!") ["Hello", "World!"]
-- |    = Just ["Hello", "World"]
-- |
-- | alterAt 1 (stripSuffix $ Pattern "!!!!!") ["Hello", "World!"]
-- |    = Just ["Hello"]
-- |
-- | alterAt 10 (stripSuffix $ Pattern "!") ["Hello", "World!"] = Nothing
-- | ```
-- |
alterAt :: forall a. Int -> (a -> Maybe a) -> Array a -> Maybe (Array a)
alterAt i f xs = maybe Nothing go (xs !! i)
  where
  go x = case f x of
    Nothing -> deleteAt i xs
    Just x' -> updateAt i x' xs

--------------------------------------------------------------------------------
-- Transformations -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Inserts the given element in between each element in the array. The array
-- | must have two or more elements for this operation to take effect.
-- |
-- | ```purescript
-- | intersperse " " [ "a", "b" ] == [ "a", " ", "b" ]
-- | intersperse 0 [ 1, 2, 3, 4, 5 ] == [ 1, 0, 2, 0, 3, 0, 4, 0, 5 ]
-- | ```
-- |
-- | If the array has less than two elements, the input array is returned.
-- | ```purescript
-- | intersperse " " [] == []
-- | intersperse " " ["a"] == ["a"]
-- | ```
intersperse :: forall a. a -> Array a -> Array a
intersperse a arr = case length arr of
  len | len < 2 -> arr
      | otherwise -> STA.run do
          let unsafeGetElem idx = unsafePartial (unsafeIndex arr idx)
          out <- STA.new
          _ <- STA.push (unsafeGetElem 0) out
          ST.for 1 len \idx -> do
            _ <- STA.push a out
            void (STA.push (unsafeGetElem idx) out)
          pure out

-- | Reverse an array, creating a new array.
-- |
-- | ```purescript
-- | reverse [] = []
-- | reverse [1, 2, 3] = [3, 2, 1]
-- | ```
-- |
foreign import reverse :: forall a. Array a -> Array a

-- | Flatten an array of arrays, creating a new array.
-- |
-- | ```purescript
-- | concat [[1, 2, 3], [], [4, 5, 6]] = [1, 2, 3, 4, 5, 6]
-- | ```
-- |
foreign import concat :: forall a. Array (Array a) -> Array a

-- | Apply a function to each element in an array, and flatten the results
-- | into a single, new array.
-- |
-- | ```purescript
-- | concatMap (split $ Pattern " ") ["Hello World", "other thing"]
-- |    = ["Hello", "World", "other", "thing"]
-- | ```
-- |
concatMap :: forall a b. (a -> Array b) -> Array a -> Array b
concatMap = flip bind

-- | Filter an array, keeping the elements which satisfy a predicate function,
-- | creating a new array.
-- |
-- | ```purescript
-- | filter (_ > 0) [-1, 4, -5, 7] = [4, 7]
-- | ```
-- |
foreign import filter :: forall a. (a -> Boolean) -> Array a -> Array a

-- | Partition an array using a predicate function, creating a set of
-- | new arrays. One for the values satisfying the predicate function
-- | and one for values that don't.
-- |
-- | ```purescript
-- | partition (_ > 0) [-1, 4, -5, 7] = { yes: [4, 7], no: [-1, -5] }
-- | ```
-- |
foreign import partition
  :: forall a
   . (a -> Boolean)
  -> Array a
  -> { yes :: Array a, no :: Array a }

-- | Splits an array into two subarrays, where `before` contains the elements
-- | up to (but not including) the given index, and `after` contains the rest
-- | of the elements, from that index on.
-- |
-- | ```purescript
-- | >>> splitAt 3 [1, 2, 3, 4, 5]
-- | { before: [1, 2, 3], after: [4, 5] }
-- | ```
-- |
-- | Thus, the length of `(splitAt i arr).before` will equal either `i` or
-- | `length arr`, if that is shorter. (Or if `i` is negative the length will
-- | be 0.)
-- |
-- | ```purescript
-- | splitAt 2 ([] :: Array Int) == { before: [], after: [] }
-- | splitAt 3 [1, 2, 3, 4, 5] == { before: [1, 2, 3], after: [4, 5] }
-- | ```
splitAt :: forall a. Int -> Array a -> { before :: Array a, after :: Array a }
splitAt i xs | i <= 0 = { before: [], after: xs }
splitAt i xs = { before: slice 0 i xs, after: slice i (length xs) xs }

-- | Filter where the predicate returns a `Boolean` in some `Applicative`.
-- |
-- | ```purescript
-- | powerSet :: forall a. Array a -> Array (Array a)
-- | powerSet = filterA (const [true, false])
-- | ```
filterA :: forall a f. Applicative f => (a -> f Boolean) -> Array a -> f (Array a)
filterA p =
  traverse (\x -> Tuple x <$> p x)
  >>> map (mapMaybe (\(Tuple x b) -> if b then Just x else Nothing))

-- | Apply a function to each element in an array, keeping only the results
-- | which contain a value, creating a new array.
-- |
-- | ```purescript
-- | parseEmail :: String -> Maybe Email
-- | parseEmail = ...
-- |
-- | mapMaybe parseEmail ["a.com", "hello@example.com", "--"]
-- |    = [Email {user: "hello", domain: "example.com"}]
-- | ```
-- |
mapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Array b
mapMaybe f = concatMap (maybe [] singleton <<< f)

-- | Filter an array of optional values, keeping only the elements which contain
-- | a value, creating a new array.
-- |
-- | ```purescript
-- | catMaybes [Nothing, Just 2, Nothing, Just 4] = [2, 4]
-- | ```
-- |
catMaybes :: forall a. Array (Maybe a) -> Array a
catMaybes = mapMaybe identity

-- | Apply a function to each element in an array, supplying a generated
-- | zero-based index integer along with the element, creating an array
-- | with the new elements.
-- |
-- | ```purescript
-- | prefixIndex index element = show index <> element
-- |
-- | mapWithIndex prefixIndex ["Hello", "World"] = ["0Hello", "1World"]
-- | ```
-- |
mapWithIndex :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapWithIndex = FWI.mapWithIndex

-- | Change the elements at the specified indices in index/value pairs.
-- | Out-of-bounds indices will have no effect.
-- |
-- | ```purescript
-- | updates = [Tuple 0 "Hi", Tuple 2 "." , Tuple 10 "foobar"]
-- |
-- | updateAtIndices updates ["Hello", "World", "!"] = ["Hi", "World", "."]
-- | ```
-- |
updateAtIndices :: forall t a. Foldable t => t (Tuple Int a) -> Array a -> Array a
updateAtIndices us xs =
  ST.run (STA.withArray (\res -> traverse_ (\(Tuple i a) -> STA.poke i a res) us) xs)

-- | Apply a function to the element at the specified indices,
-- | creating a new array. Out-of-bounds indices will have no effect.
-- |
-- | ```purescript
-- | indices = [1, 3]
-- | modifyAtIndices indices toUpper ["Hello", "World", "and", "others"]
-- |    = ["Hello", "WORLD", "and", "OTHERS"]
-- | ```
-- |
modifyAtIndices :: forall t a. Foldable t => t Int -> (a -> a) -> Array a -> Array a
modifyAtIndices is f xs =
  ST.run (STA.withArray (\res -> traverse_ (\i -> STA.modify i f res) is) xs)

foldl :: forall a b. (b -> a -> b) -> b -> Array a -> b
foldl = F.foldl

foldr :: forall a b. (a -> b -> b) -> b -> Array a -> b
foldr = F.foldr

foldMap :: forall a m. Monoid m => (a -> m) -> Array a -> m
foldMap = F.foldMap

fold :: forall m. Monoid m => Array m -> m
fold = F.fold

intercalate :: forall a. Monoid a => a -> Array a -> a
intercalate = F.intercalate

-- | The 'transpose' function transposes the rows and columns of its argument.
-- | For example,
-- |
-- | ```purescript
-- | transpose 
-- |   [ [1, 2, 3]
-- |   , [4, 5, 6]
-- |   ] == 
-- |   [ [1, 4]
-- |   , [2, 5]
-- |   , [3, 6]
-- |   ]
-- | ```
-- |
-- | If some of the rows are shorter than the following rows, their elements are skipped:
-- |
-- | ```purescript
-- | transpose 
-- |   [ [10, 11]
-- |   , [20]
-- |   , [30, 31, 32]
-- |   ] == 
-- |   [ [10, 20, 30]
-- |   , [11, 31]
-- |   , [32]
-- |   ]
-- | ```
transpose :: forall a. Array (Array a) -> Array (Array a)
transpose xs = go 0 []
  where
  go :: Int -> Array (Array a) -> Array (Array a)
  go idx allArrays = case buildNext idx of
    Nothing -> allArrays
    Just next -> go (idx + 1) (snoc allArrays next)  
   
  buildNext :: Int -> Maybe (Array a)
  buildNext idx = do
    xs # flip foldl Nothing \acc nextArr -> do
      maybe acc (\el -> Just $ maybe [el] (flip snoc el) acc) $ index nextArr idx

-- | Fold a data structure from the left, keeping all intermediate results
-- | instead of only the final result. Note that the initial value does not
-- | appear in the result (unlike Haskell's `Prelude.scanl`).
-- |
-- | ```
-- | scanl (+) 0  [1,2,3] = [1,3,6]
-- | scanl (-) 10 [1,2,3] = [9,7,4]
-- | ```
foreign import scanl :: forall a b. (b -> a -> b) -> b -> Array a -> Array b

-- | Fold a data structure from the right, keeping all intermediate results
-- | instead of only the final result. Note that the initial value does not
-- | appear in the result (unlike Haskell's `Prelude.scanr`).
-- |
-- | ```
-- | scanr (+) 0 [1,2,3] = [6,5,3]
-- | scanr (flip (-)) 10 [1,2,3] = [4,5,7]
-- | ```
foreign import scanr :: forall a b. (a -> b -> b) -> b -> Array a -> Array b

--------------------------------------------------------------------------------
-- Sorting ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Sort the elements of an array in increasing order, creating a new array.
-- | Sorting is stable: the order of equal elements is preserved.
-- |
-- | ```purescript
-- | sort [2, -3, 1] = [-3, 1, 2]
-- | ```
-- |
sort :: forall a. Ord a => Array a -> Array a
sort xs = sortBy compare xs

-- | Sort the elements of an array in increasing order, where elements are
-- | compared using the specified partial ordering, creating a new array.
-- | Sorting is stable: the order of elements is preserved if they are equal
-- | according to the specified partial ordering.
-- |
-- | ```purescript
-- | compareLength a b = compare (length a) (length b)
-- | sortBy compareLength [[1, 2, 3], [7, 9], [-2]] = [[-2],[7,9],[1,2,3]]
-- | ```
-- |
sortBy :: forall a. (a -> a -> Ordering) -> Array a -> Array a
sortBy comp = sortByImpl comp case _ of
  GT -> 1
  EQ -> 0
  LT -> -1

-- | Sort the elements of an array in increasing order, where elements are
-- | sorted based on a projection. Sorting is stable: the order of elements is
-- | preserved if they are equal according to the projection.
-- |
-- | ```purescript
-- | sortWith (_.age) [{name: "Alice", age: 42}, {name: "Bob", age: 21}]
-- |    = [{name: "Bob", age: 21}, {name: "Alice", age: 42}]
-- | ```
-- |
sortWith :: forall a b. Ord b => (a -> b) -> Array a -> Array a
sortWith f = sortBy (comparing f)

foreign import sortByImpl :: forall a. (a -> a -> Ordering) -> (Ordering -> Int) -> Array a -> Array a

--------------------------------------------------------------------------------
-- Subarrays -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Extract a subarray by a start and end index.
-- |
-- | ```purescript
-- | letters = ["a", "b", "c"]
-- | slice 1 3 letters = ["b", "c"]
-- | slice 5 7 letters = []
-- | slice 4 1 letters = []
-- | ```
-- |
foreign import slice :: forall a. Int -> Int -> Array a -> Array a

-- | Keep only a number of elements from the start of an array, creating a new
-- | array.
-- |
-- | ```purescript
-- | letters = ["a", "b", "c"]
-- |
-- | take 2 letters = ["a", "b"]
-- | take 100 letters = ["a", "b", "c"]
-- | ```
-- |
take :: forall a. Int -> Array a -> Array a
take n xs = if n < 1 then [] else slice 0 n xs

-- | Keep only a number of elements from the end of an array, creating a new
-- | array.
-- |
-- | ```purescript
-- | letters = ["a", "b", "c"]
-- |
-- | takeEnd 2 letters = ["b", "c"]
-- | takeEnd 100 letters = ["a", "b", "c"]
-- | ```
-- |
takeEnd :: forall a. Int -> Array a -> Array a
takeEnd n xs = drop (length xs - n) xs

-- | Calculate the longest initial subarray for which all element satisfy the
-- | specified predicate, creating a new array.
-- |
-- | ```purescript
-- | takeWhile (_ > 0) [4, 1, 0, -4, 5] = [4, 1]
-- | takeWhile (_ > 0) [-1, 4] = []
-- | ```
-- |
takeWhile :: forall a. (a -> Boolean) -> Array a -> Array a
takeWhile p xs = (span p xs).init

-- | Drop a number of elements from the start of an array, creating a new array.
-- |
-- | ```purescript
-- | letters = ["a", "b", "c", "d"]
-- |
-- | drop 2 letters = ["c", "d"]
-- | drop 10 letters = []
-- | ```
-- |
drop :: forall a. Int -> Array a -> Array a
drop n xs = if n < 1 then xs else slice n (length xs) xs

-- | Drop a number of elements from the end of an array, creating a new array.
-- |
-- | ```purescript
-- | letters = ["a", "b", "c", "d"]
-- |
-- | dropEnd 2 letters = ["a", "b"]
-- | dropEnd 10 letters = []
-- | ```
-- |
dropEnd :: forall a. Int -> Array a -> Array a
dropEnd n xs = take (length xs - n) xs

-- | Remove the longest initial subarray for which all element satisfy the
-- | specified predicate, creating a new array.
-- |
-- | ```purescript
-- | dropWhile (_ < 0) [-3, -1, 0, 4, -6] = [0, 4, -6]
-- | ```
-- |
dropWhile :: forall a. (a -> Boolean) -> Array a -> Array a
dropWhile p xs = (span p xs).rest

-- | Split an array into two parts:
-- |
-- | 1. the longest initial subarray for which all elements satisfy the
-- |    specified predicate
-- | 2. the remaining elements
-- |
-- | ```purescript
-- | span (\n -> n % 2 == 1) [1,3,2,4,5] == { init: [1,3], rest: [2,4,5] }
-- | ```
-- |
-- | Running time: `O(n)`.
span
  :: forall a
   . (a -> Boolean)
  -> Array a
  -> { init :: Array a, rest :: Array a }
span p arr =
  case breakIndex of
    Just 0 ->
      { init: [], rest: arr }
    Just i ->
      { init: slice 0 i arr, rest: slice i (length arr) arr }
    Nothing ->
      { init: arr, rest: [] }
  where
  breakIndex = go 0
  go i =
    -- This looks like a good opportunity to use the Monad Maybe instance,
    -- but it's important to write out an explicit case expression here in
    -- order to ensure that TCO is triggered.
    case index arr i of
      Just x -> if p x then go (i + 1) else Just i
      Nothing -> Nothing

-- | Group equal, consecutive elements of an array into arrays.
-- |
-- | ```purescript
-- | group [1, 1, 2, 2, 1] == [NonEmptyArray [1, 1], NonEmptyArray [2, 2], NonEmptyArray [1]]
-- | ```
group :: forall a. Eq a => Array a -> Array (NonEmptyArray a)
group xs = groupBy eq xs

-- | Group equal elements of an array into arrays.
-- |
-- | ```purescript
-- | groupAll [1, 1, 2, 2, 1] == [NonEmptyArray [1, 1, 1], NonEmptyArray [2, 2]]
-- | ```
groupAll :: forall a. Ord a => Array a -> Array (NonEmptyArray a)
groupAll = groupAllBy compare

-- | Group equal, consecutive elements of an array into arrays, using the
-- | specified equivalence relation to determine equality.
-- |
-- | ```purescript
-- | groupBy (\a b -> odd a && odd b) [1, 3, 2, 4, 3, 3]
-- |    = [NonEmptyArray [1, 3], NonEmptyArray [2], NonEmptyArray [4], NonEmptyArray [3, 3]]
-- | ```
-- |
groupBy :: forall a. (a -> a -> Boolean) -> Array a -> Array (NonEmptyArray a)
groupBy op xs =
  ST.run do
    result <- STA.new
    iter <- STAI.iterator (xs !! _)
    STAI.iterate iter \x -> void do
      sub <- STA.new
      _ <- STA.push x sub
      STAI.pushWhile (op x) iter sub
      grp <- STA.unsafeFreeze sub
      STA.push (NonEmptyArray grp) result
    STA.unsafeFreeze result

-- | Group equal elements of an array into arrays, using the specified
-- | comparison function to determine equality.
-- |
-- | ```purescript
-- | groupAllBy (comparing Down) [1, 3, 2, 4, 3, 3]
-- |    = [NonEmptyArray [4], NonEmptyArray [3, 3, 3], NonEmptyArray [2], NonEmptyArray [1]]
-- | ```
-- |
groupAllBy :: forall a. (a -> a -> Ordering) -> Array a -> Array (NonEmptyArray a)
groupAllBy cmp = groupBy (\x y -> cmp x y == EQ) <<< sortBy cmp

-- | Remove the duplicates from an array, creating a new array.
-- |
-- | ```purescript
-- | nub [1, 2, 1, 3, 3] = [1, 2, 3]
-- | ```
-- |
nub :: forall a. Ord a => Array a -> Array a
nub = nubBy compare

-- | Remove the duplicates from an array, creating a new array.
-- |
-- | This less efficient version of `nub` only requires an `Eq` instance.
-- |
-- | ```purescript
-- | nubEq [1, 2, 1, 3, 3] = [1, 2, 3]
-- | ```
-- |
nubEq :: forall a. Eq a => Array a -> Array a
nubEq = nubByEq eq

-- | Remove the duplicates from an array, where element equality is determined
-- | by the specified ordering, creating a new array.
-- |
-- | ```purescript
-- | nubBy compare [1, 3, 4, 2, 2, 1] == [1, 3, 4, 2]
-- | ```
-- |
nubBy :: forall a. (a -> a -> Ordering) -> Array a -> Array a
nubBy comp xs = case head indexedAndSorted of
  Nothing -> []
  Just x -> map snd $ sortWith fst $ ST.run do
     -- TODO: use NonEmptyArrays here to avoid partial functions
     result <- STA.unsafeThaw $ singleton x
     ST.foreach indexedAndSorted \pair@(Tuple _ x') -> do
       lst <- snd <<< unsafePartial (fromJust <<< last) <$> STA.unsafeFreeze result
       when (comp lst x' /= EQ) $ void $ STA.push pair result
     STA.unsafeFreeze result
  where
  indexedAndSorted :: Array (Tuple Int a)
  indexedAndSorted = sortBy (\x y -> comp (snd x) (snd y))
                            (mapWithIndex Tuple xs)

-- | Remove the duplicates from an array, where element equality is determined
-- | by the specified equivalence relation, creating a new array.
-- |
-- | This less efficient version of `nubBy` only requires an equivalence
-- | relation.
-- |
-- | ```purescript
-- | mod3eq a b = a `mod` 3 == b `mod` 3
-- | nubByEq mod3eq [1, 3, 4, 5, 6] = [1, 3, 5]
-- | ```
-- |
nubByEq :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubByEq eq xs = ST.run do
  arr <- STA.new
  ST.foreach xs \x -> do
    e <- not <<< any (_ `eq` x) <$> (STA.unsafeFreeze arr)
    when e $ void $ STA.push x arr
  STA.unsafeFreeze arr

-- | Calculate the union of two arrays. Note that duplicates in the first array
-- | are preserved while duplicates in the second array are removed.
-- |
-- | Running time: `O(n^2)`
-- |
-- | ```purescript
-- | union [1, 2, 1, 1] [3, 3, 3, 4] = [1, 2, 1, 1, 3, 4]
-- | ```
-- |
union :: forall a. Eq a => Array a -> Array a -> Array a
union = unionBy (==)

-- | Calculate the union of two arrays, using the specified function to
-- | determine equality of elements. Note that duplicates in the first array
-- | are preserved while duplicates in the second array are removed.
-- |
-- | ```purescript
-- | mod3eq a b = a `mod` 3 == b `mod` 3
-- | unionBy mod3eq [1, 5, 1, 2] [3, 4, 3, 3] = [1, 5, 1, 2, 3]
-- | ```
-- |
unionBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
unionBy eq xs ys = xs <> foldl (flip (deleteBy eq)) (nubByEq eq ys) xs

-- | Delete the first element of an array which is equal to the specified value,
-- | creating a new array.
-- |
-- | ```purescript
-- | delete 7 [1, 7, 3, 7] = [1, 3, 7]
-- | delete 7 [1, 2, 3] = [1, 2, 3]
-- | ```
-- |
-- | Running time: `O(n)`
delete :: forall a. Eq a => a -> Array a -> Array a
delete = deleteBy eq

-- | Delete the first element of an array which matches the specified value,
-- | under the equivalence relation provided in the first argument, creating a
-- | new array.
-- |
-- | ```purescript
-- | mod3eq a b = a `mod` 3 == b `mod` 3
-- | deleteBy mod3eq 6 [1, 3, 4, 3] = [1, 4, 3]
-- | ```
-- |
deleteBy :: forall a. (a -> a -> Boolean) -> a -> Array a -> Array a
deleteBy _  _ [] = []
deleteBy eq x ys = maybe ys (\i -> unsafePartial $ fromJust (deleteAt i ys)) (findIndex (eq x) ys)

-- | Delete the first occurrence of each element in the second array from the
-- | first array, creating a new array.
-- |
-- | ```purescript
-- | difference [2, 1] [2, 3] = [1]
-- | ```
-- |
-- | Running time: `O(n*m)`, where n is the length of the first array, and m is
-- | the length of the second.
difference :: forall a. Eq a => Array a -> Array a -> Array a
difference = foldr delete

infix 5 difference as \\

-- | Calculate the intersection of two arrays, creating a new array. Note that
-- | duplicates in the first array are preserved while duplicates in the second
-- | array are removed.
-- |
-- | ```purescript
-- | intersect [1, 1, 2] [2, 2, 1] = [1, 1, 2]
-- | ```
-- |
intersect :: forall a. Eq a => Array a -> Array a -> Array a
intersect = intersectBy eq

-- | Calculate the intersection of two arrays, using the specified equivalence
-- | relation to compare elements, creating a new array. Note that duplicates
-- | in the first array are preserved while duplicates in the second array are
-- | removed.
-- |
-- | ```purescript
-- | mod3eq a b = a `mod` 3 == b `mod` 3
-- | intersectBy mod3eq [1, 2, 3] [4, 6, 7] = [1, 3]
-- | ```
-- |
intersectBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
intersectBy eq xs ys = filter (\x -> isJust (findIndex (eq x) ys)) xs

-- | Apply a function to pairs of elements at the same index in two arrays,
-- | collecting the results in a new array.
-- |
-- | If one array is longer, elements will be discarded from the longer array.
-- |
-- | For example
-- |
-- | ```purescript
-- | zipWith (*) [1, 2, 3] [4, 5, 6, 7] == [4, 10, 18]
-- | ```
foreign import zipWith
  :: forall a b c
   . (a -> b -> c)
  -> Array a
  -> Array b
  -> Array c

-- | A generalization of `zipWith` which accumulates results in some
-- | `Applicative` functor.
-- |
-- | ```purescript
-- | sndChars = zipWithA (\a b -> charAt 2 (a <> b))
-- | sndChars ["a", "b"] ["A", "B"] = Nothing -- since "aA" has no 3rd char
-- | sndChars ["aa", "b"] ["AA", "BBB"] = Just ['A', 'B']
-- | ```
-- |
zipWithA
  :: forall m a b c
   . Applicative m
  => (a -> b -> m c)
  -> Array a
  -> Array b
  -> m (Array c)
zipWithA f xs ys = sequence (zipWith f xs ys)

-- | Takes two arrays and returns an array of corresponding pairs.
-- | If one input array is short, excess elements of the longer array are
-- | discarded.
-- |
-- | ```purescript
-- | zip [1, 2, 3] ["a", "b"] = [Tuple 1 "a", Tuple 2 "b"]
-- | ```
-- |
zip :: forall a b. Array a -> Array b -> Array (Tuple a b)
zip = zipWith Tuple

-- | Transforms an array of pairs into an array of first components and an
-- | array of second components.
-- |
-- | ```purescript
-- | unzip [Tuple 1 "a", Tuple 2 "b"] = Tuple [1, 2] ["a", "b"]
-- | ```
-- |
unzip :: forall a b. Array (Tuple a b) -> Tuple (Array a) (Array b)
unzip xs =
  ST.run do
    fsts <- STA.new
    snds <- STA.new
    iter <- STAI.iterator (xs !! _)
    STAI.iterate iter \(Tuple fst snd) -> do
      void $ STA.push fst fsts
      void $ STA.push snd snds
    fsts' <- STA.unsafeFreeze fsts
    snds' <- STA.unsafeFreeze snds
    pure $ Tuple fsts' snds'

-- | Returns true if at least one array element satisfies the given predicate,
-- | iterating the array only as necessary and stopping as soon as the predicate
-- | yields true.
-- |
-- | ```purescript
-- | any (_ > 0) [] = False
-- | any (_ > 0) [-1, 0, 1] = True
-- | any (_ > 0) [-1, -2, -3] = False
-- | ```
foreign import any :: forall a. (a -> Boolean) -> Array a -> Boolean

-- | Returns true if all the array elements satisfy the given predicate.
-- | iterating the array only as necessary and stopping as soon as the predicate
-- | yields false.
-- |
-- | ```purescript
-- | all (_ > 0) [] = True
-- | all (_ > 0) [1, 2, 3] = True
-- | all (_ > 0) [-1, -2, -3] = False
-- | ```
foreign import all :: forall a. (a -> Boolean) -> Array a -> Boolean

-- | Perform a fold using a monadic step function.
-- |
-- | ```purescript
-- | foldM (\x y -> Just (x + y)) 0 [1, 4] = Just 5
-- | ```
foldM :: forall m a b. Monad m => (b -> a -> m b) -> b -> Array a -> m b
foldM f b = unconsImpl (\_ -> pure b) (\a as -> f b a >>= \b' -> foldM f b' as)

foldRecM :: forall m a b. MonadRec m => (b -> a -> m b) -> b -> Array a -> m b
foldRecM f b array = tailRecM2 go b 0
  where
  go res i
    | i >= length array = pure (Done res)
    | otherwise = do
        res' <- f res (unsafePartial (unsafeIndex array i))
        pure (Loop { a: res', b: i + 1 })

-- | Find the element of an array at the specified index.
-- |
-- | ```purescript
-- | unsafePartial $ unsafeIndex ["a", "b", "c"] 1 = "b"
-- | ```
-- |
-- | Using `unsafeIndex` with an out-of-range index will not immediately raise a runtime error.
-- | Instead, the result will be undefined. Most attempts to subsequently use the result will
-- | cause a runtime error, of course, but this is not guaranteed, and is dependent on the backend;
-- | some programs will continue to run as if nothing is wrong. For example, in the JavaScript backend,
-- | the expression `unsafePartial (unsafeIndex [true] 1)` has type `Boolean`;
-- | since this expression evaluates to `undefined`, attempting to use it in an `if` statement will cause
-- | the else branch to be taken.
unsafeIndex :: forall a. Partial => Array a -> Int -> a
unsafeIndex = unsafeIndexImpl

foreign import unsafeIndexImpl :: forall a. Array a -> Int -> a
