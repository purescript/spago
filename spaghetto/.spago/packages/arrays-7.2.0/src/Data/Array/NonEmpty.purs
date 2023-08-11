module Data.Array.NonEmpty
  ( module Internal
  , fromArray
  , fromNonEmpty
  , toArray
  , toNonEmpty

  , fromFoldable
  , fromFoldable1
  , toUnfoldable
  , toUnfoldable1
  , singleton
  , (..), range
  , replicate
  , some

  , length

  , (:), cons
  , cons'
  , snoc
  , snoc'
  , appendArray
  , prependArray
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
  , foldl1
  , foldr1
  , foldMap1
  , fold1
  , intercalate
  , transpose
  , transpose'
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
  , nubBy
  , nubEq
  , nubByEq
  , union
  , union'
  , unionBy
  , unionBy'
  , delete
  , deleteBy

  , (\\), difference
  , difference'
  , intersect
  , intersect'
  , intersectBy
  , intersectBy'

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

import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as A
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Array.NonEmpty.Internal (NonEmptyArray) as Internal
import Data.Bifunctor (bimap)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Foldable as F
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

-- | Internal - adapt an Array transform to NonEmptyArray
--
-- Note that this is unsafe: if the transform returns an empty array, this can
-- explode at runtime.
unsafeAdapt :: forall a b. (Array a -> Array b) -> NonEmptyArray a -> NonEmptyArray b
unsafeAdapt f = unsafeFromArray <<< adaptAny f

-- | Internal - adapt an Array transform to NonEmptyArray,
--   with polymorphic result.
--
-- Note that this is unsafe: if the transform returns an empty array, this can
-- explode at runtime.
adaptAny :: forall a b. (Array a -> b) -> NonEmptyArray a -> b
adaptAny f = f <<< toArray

-- | Internal - adapt Array functions returning Maybes to NonEmptyArray
adaptMaybe :: forall a b. (Array a -> Maybe b) -> NonEmptyArray a -> b
adaptMaybe f = unsafePartial $ fromJust <<< f <<< toArray

fromArray :: forall a. Array a -> Maybe (NonEmptyArray a)
fromArray xs
  | A.length xs > 0 = Just (unsafeFromArray xs)
  | otherwise = Nothing

-- | INTERNAL
unsafeFromArray :: forall a. Array a -> NonEmptyArray a
unsafeFromArray = NonEmptyArray

unsafeFromArrayF :: forall f a. f (Array a) -> f (NonEmptyArray a)
unsafeFromArrayF = unsafeCoerce

fromNonEmpty :: forall a. NonEmpty Array a -> NonEmptyArray a
fromNonEmpty (x :| xs) = cons' x xs

toArray :: forall a. NonEmptyArray a -> Array a
toArray (NonEmptyArray xs) = xs

toNonEmpty :: forall a. NonEmptyArray a -> NonEmpty Array a
toNonEmpty = uncons >>> \{head: x, tail: xs} -> x :| xs

fromFoldable :: forall f a. Foldable f => f a -> Maybe (NonEmptyArray a)
fromFoldable = fromArray <<< A.fromFoldable

fromFoldable1 :: forall f a. Foldable1 f => f a -> NonEmptyArray a
fromFoldable1 = unsafeFromArray <<< A.fromFoldable

toUnfoldable :: forall f a. Unfoldable f => NonEmptyArray a -> f a
toUnfoldable = adaptAny A.toUnfoldable

toUnfoldable1 :: forall f a. Unfoldable1 f => NonEmptyArray a -> f a
toUnfoldable1 xs = unfoldr1 f 0
  where
  len = length xs
  f i = Tuple (unsafePartial unsafeIndex xs i) $
          if i < (len - 1) then Just (i + 1) else Nothing

singleton :: forall a. a -> NonEmptyArray a
singleton = unsafeFromArray <<< A.singleton

range :: Int -> Int -> NonEmptyArray Int
range x y = unsafeFromArray $ A.range x y

infix 8 range as ..

-- | Replicate an item at least once
replicate :: forall a. Int -> a -> NonEmptyArray a
replicate i x = unsafeFromArray $ A.replicate (max 1 i) x

some
  :: forall f a
   . Alternative f
  => Lazy (f (Array a))
  => f a -> f (NonEmptyArray a)
some = unsafeFromArrayF <<< A.some

length :: forall a. NonEmptyArray a -> Int
length = adaptAny A.length

cons :: forall a. a -> NonEmptyArray a -> NonEmptyArray a
cons x = unsafeAdapt $ A.cons x

infixr 6 cons as :

cons' :: forall a. a -> Array a -> NonEmptyArray a
cons' x xs = unsafeFromArray $ A.cons x xs

snoc :: forall a. NonEmptyArray a -> a -> NonEmptyArray a
snoc xs x = unsafeFromArray $ A.snoc (toArray xs) x

snoc' :: forall a. Array a -> a -> NonEmptyArray a
snoc' xs x = unsafeFromArray $ A.snoc xs x

appendArray :: forall a. NonEmptyArray a -> Array a -> NonEmptyArray a
appendArray xs ys = unsafeFromArray $ toArray xs <> ys

prependArray :: forall a. Array a -> NonEmptyArray a -> NonEmptyArray a
prependArray xs ys = unsafeFromArray $ xs <> toArray ys

insert :: forall a. Ord a => a -> NonEmptyArray a -> NonEmptyArray a
insert x = unsafeAdapt $ A.insert x

insertBy :: forall a. (a -> a -> Ordering) -> a -> NonEmptyArray a -> NonEmptyArray a
insertBy f x = unsafeAdapt $ A.insertBy f x

head :: forall a. NonEmptyArray a -> a
head = adaptMaybe A.head

last :: forall a. NonEmptyArray a -> a
last = adaptMaybe A.last

tail :: forall a. NonEmptyArray a -> Array a
tail = adaptMaybe A.tail

init :: forall a. NonEmptyArray a -> Array a
init = adaptMaybe A.init

uncons :: forall a. NonEmptyArray a -> { head :: a, tail :: Array a }
uncons = adaptMaybe A.uncons

unsnoc :: forall a. NonEmptyArray a -> { init :: Array a, last :: a }
unsnoc = adaptMaybe A.unsnoc

index :: forall a. NonEmptyArray a -> Int -> Maybe a
index = adaptAny A.index

infixl 8 index as !!

elem :: forall a. Eq a => a -> NonEmptyArray a -> Boolean
elem x = adaptAny $ A.elem x

notElem :: forall a. Eq a => a -> NonEmptyArray a -> Boolean
notElem x = adaptAny $ A.notElem x

elemIndex :: forall a. Eq a => a -> NonEmptyArray a -> Maybe Int
elemIndex x = adaptAny $ A.elemIndex x

elemLastIndex :: forall a. Eq a => a -> NonEmptyArray a -> Maybe Int
elemLastIndex x = adaptAny $ A.elemLastIndex x

find :: forall a. (a -> Boolean) -> NonEmptyArray a -> Maybe a
find p = adaptAny $ A.find p

findMap :: forall a b. (a -> Maybe b) -> NonEmptyArray a -> Maybe b
findMap p = adaptAny $ A.findMap p

findIndex :: forall a. (a -> Boolean) -> NonEmptyArray a -> Maybe Int
findIndex p = adaptAny $ A.findIndex p

findLastIndex :: forall a. (a -> Boolean) -> NonEmptyArray a -> Maybe Int
findLastIndex x = adaptAny $ A.findLastIndex x

insertAt :: forall a. Int -> a -> NonEmptyArray a -> Maybe (NonEmptyArray a)
insertAt i x = unsafeFromArrayF <<< A.insertAt i x <<< toArray

deleteAt :: forall a. Int -> NonEmptyArray a -> Maybe (Array a)
deleteAt i = adaptAny $ A.deleteAt i

updateAt :: forall a. Int -> a -> NonEmptyArray a -> Maybe (NonEmptyArray a)
updateAt i x = unsafeFromArrayF <<< A.updateAt i x <<< toArray

updateAtIndices :: forall t a. Foldable t => t (Tuple Int a) -> NonEmptyArray a -> NonEmptyArray a
updateAtIndices pairs = unsafeAdapt $ A.updateAtIndices pairs

modifyAt :: forall a. Int -> (a -> a) -> NonEmptyArray a -> Maybe (NonEmptyArray a)
modifyAt i f = unsafeFromArrayF <<< A.modifyAt i f <<< toArray

modifyAtIndices :: forall t a. Foldable t => t Int -> (a -> a) -> NonEmptyArray a -> NonEmptyArray a
modifyAtIndices is f = unsafeAdapt $ A.modifyAtIndices is f

alterAt :: forall a. Int -> (a -> Maybe a) -> NonEmptyArray a -> Maybe (Array a)
alterAt i f = A.alterAt i f <<< toArray

intersperse :: forall a. a -> NonEmptyArray a -> NonEmptyArray a
intersperse x = unsafeAdapt $ A.intersperse x

reverse :: forall a. NonEmptyArray a -> NonEmptyArray a
reverse = unsafeAdapt A.reverse

concat :: forall a. NonEmptyArray (NonEmptyArray a) -> NonEmptyArray a
concat = unsafeFromArray <<< A.concat <<< toArray <<< map toArray

concatMap :: forall a b. (a -> NonEmptyArray b) -> NonEmptyArray a -> NonEmptyArray b
concatMap = flip bind

filter :: forall a. (a -> Boolean) -> NonEmptyArray a -> Array a
filter f = adaptAny $ A.filter f

partition
  :: forall a
   . (a -> Boolean)
  -> NonEmptyArray a
  -> { yes :: Array a, no :: Array a}
partition f = adaptAny $ A.partition f

filterA
  :: forall a f
   . Applicative f
  => (a -> f Boolean)
  -> NonEmptyArray a
  -> f (Array a)
filterA f = adaptAny $ A.filterA f

splitAt :: forall a. Int -> NonEmptyArray a -> { before :: Array a, after :: Array a }
splitAt i xs = A.splitAt i $ toArray xs

mapMaybe :: forall a b. (a -> Maybe b) -> NonEmptyArray a -> Array b
mapMaybe f = adaptAny $ A.mapMaybe f

catMaybes :: forall a. NonEmptyArray (Maybe a) -> Array a
catMaybes = adaptAny A.catMaybes

mapWithIndex :: forall a b. (Int -> a -> b) -> NonEmptyArray a -> NonEmptyArray b
mapWithIndex f = unsafeAdapt $ A.mapWithIndex f

foldl1 :: forall a. (a -> a -> a) -> NonEmptyArray a -> a
foldl1 = F.foldl1

foldr1 :: forall a. (a -> a -> a) -> NonEmptyArray a -> a
foldr1 = F.foldr1

foldMap1 :: forall a m. Semigroup m => (a -> m) -> NonEmptyArray a -> m
foldMap1 = F.foldMap1

fold1 :: forall m. Semigroup m => NonEmptyArray m -> m
fold1 = F.fold1

intercalate :: forall a. Semigroup a => a -> NonEmptyArray a -> a
intercalate = F.intercalate

-- | The 'transpose' function transposes the rows and columns of its argument.
-- | For example,
-- |
-- | ```purescript
-- | transpose 
-- |   (NonEmptyArray [ NonEmptyArray [1, 2, 3]
-- |                  , NonEmptyArray [4, 5, 6]
-- |                  ]) == 
-- |   (NonEmptyArray [ NonEmptyArray [1, 4]
-- |                  , NonEmptyArray [2, 5]
-- |                  , NonEmptyArray [3, 6]
-- |                  ])
-- | ```
-- |
-- | If some of the rows are shorter than the following rows, their elements are skipped:
-- |
-- | ```purescript
-- | transpose 
-- |   (NonEmptyArray [ NonEmptyArray [10, 11]
-- |                  , NonEmptyArray [20]
-- |                  , NonEmptyArray [30, 31, 32]
-- |                  ]) == 
-- |   (NomEmptyArray [ NonEmptyArray [10, 20, 30]
-- |                  , NonEmptyArray [11, 31]
-- |                  , NonEmptyArray [32]
-- |                  ])
-- | ```
transpose :: forall a. NonEmptyArray (NonEmptyArray a) -> NonEmptyArray (NonEmptyArray a)
transpose = 
  (coerce :: (Array (Array a)) -> (NonEmptyArray (NonEmptyArray a))) 
    <<< A.transpose <<< coerce

-- | `transpose`' is identical to `transpose` other than that the inner arrays are each
-- | a standard `Array` and not a `NonEmptyArray`. However, the result is wrapped in a 
-- | `Maybe` to cater for the case where the inner `Array` is empty and must return `Nothing`.
transpose' :: forall a. NonEmptyArray (Array a) -> Maybe (NonEmptyArray (Array a))
transpose' = fromArray <<< A.transpose <<< coerce

scanl :: forall a b. (b -> a -> b) -> b -> NonEmptyArray a -> NonEmptyArray b
scanl f x = unsafeAdapt $ A.scanl f x

scanr :: forall a b. (a -> b -> b) -> b -> NonEmptyArray a -> NonEmptyArray b
scanr f x = unsafeAdapt $ A.scanr f x

sort :: forall a. Ord a => NonEmptyArray a -> NonEmptyArray a
sort = unsafeAdapt A.sort

sortBy :: forall a. (a -> a -> Ordering) -> NonEmptyArray a -> NonEmptyArray a
sortBy f = unsafeAdapt $ A.sortBy f

sortWith :: forall a b. Ord b => (a -> b) -> NonEmptyArray a -> NonEmptyArray a
sortWith f = unsafeAdapt $ A.sortWith f

slice :: forall a. Int -> Int -> NonEmptyArray a -> Array a
slice start end = adaptAny $ A.slice start end

take :: forall a. Int -> NonEmptyArray a -> Array a
take i = adaptAny $ A.take i

takeEnd :: forall a. Int -> NonEmptyArray a -> Array a
takeEnd i = adaptAny $ A.takeEnd i

takeWhile :: forall a. (a -> Boolean) -> NonEmptyArray a -> Array a
takeWhile f = adaptAny $ A.takeWhile f

drop :: forall a. Int -> NonEmptyArray a -> Array a
drop i = adaptAny $ A.drop i

dropEnd :: forall a. Int -> NonEmptyArray a -> Array a
dropEnd i = adaptAny $ A.dropEnd i

dropWhile :: forall a. (a -> Boolean) -> NonEmptyArray a -> Array a
dropWhile f = adaptAny $ A.dropWhile f

span
  :: forall a
   . (a -> Boolean)
  -> NonEmptyArray a
  -> { init :: Array a, rest :: Array a }
span f = adaptAny $ A.span f

-- | Group equal, consecutive elements of an array into arrays.
-- |
-- | ```purescript
-- | group (NonEmptyArray [1, 1, 2, 2, 1]) ==
-- |   NonEmptyArray [NonEmptyArray [1, 1], NonEmptyArray [2, 2], NonEmptyArray [1]]
-- | ```
group :: forall a. Eq a => NonEmptyArray a -> NonEmptyArray (NonEmptyArray a)
group = unsafeAdapt $ A.group

-- | Group equal elements of an array into arrays.
-- |
-- | ```purescript
-- | groupAll (NonEmptyArray [1, 1, 2, 2, 1]) ==
-- |   NonEmptyArray [NonEmptyArray [1, 1, 1], NonEmptyArray [2, 2]]
-- | `
groupAll :: forall a. Ord a => NonEmptyArray a -> NonEmptyArray (NonEmptyArray a)
groupAll = groupAllBy compare

-- | Group equal, consecutive elements of an array into arrays, using the
-- | specified equivalence relation to determine equality.
-- |
-- | ```purescript
-- | groupBy (\a b -> odd a && odd b) (NonEmptyArray [1, 3, 2, 4, 3, 3])
-- |    = NonEmptyArray [NonEmptyArray [1, 3], NonEmptyArray [2], NonEmptyArray [4], NonEmptyArray [3, 3]]
-- | ```
-- |
groupBy :: forall a. (a -> a -> Boolean) -> NonEmptyArray a -> NonEmptyArray (NonEmptyArray a)
groupBy op = unsafeAdapt $ A.groupBy op

-- | Group equal elements of an array into arrays, using the specified
-- | comparison function to determine equality.
-- |
-- | ```purescript
-- | groupAllBy (comparing Down) (NonEmptyArray [1, 3, 2, 4, 3, 3])
-- |    = NonEmptyArray [NonEmptyArray [4], NonEmptyArray [3, 3, 3], NonEmptyArray [2], NonEmptyArray [1]]
-- | ```
groupAllBy :: forall a. (a -> a -> Ordering) -> NonEmptyArray a -> NonEmptyArray (NonEmptyArray a)
groupAllBy op = unsafeAdapt $ A.groupAllBy op

nub :: forall a. Ord a => NonEmptyArray a -> NonEmptyArray a
nub = unsafeAdapt A.nub

nubEq :: forall a. Eq a => NonEmptyArray a -> NonEmptyArray a
nubEq = unsafeAdapt A.nubEq

nubBy :: forall a. (a -> a -> Ordering) -> NonEmptyArray a -> NonEmptyArray a
nubBy f = unsafeAdapt $ A.nubBy f

nubByEq :: forall a. (a -> a -> Boolean) -> NonEmptyArray a -> NonEmptyArray a
nubByEq f = unsafeAdapt $ A.nubByEq f

union :: forall a. Eq a => NonEmptyArray a -> NonEmptyArray a -> NonEmptyArray a
union = unionBy (==)

union' :: forall a. Eq a => NonEmptyArray a -> Array a -> NonEmptyArray a
union' = unionBy' (==)

unionBy
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArray a
  -> NonEmptyArray a
  -> NonEmptyArray a
unionBy eq xs = unionBy' eq xs <<< toArray

unionBy'
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArray a
  -> Array a
  -> NonEmptyArray a
unionBy' eq xs = unsafeFromArray <<< A.unionBy eq (toArray xs)

delete :: forall a. Eq a => a -> NonEmptyArray a -> Array a
delete x = adaptAny $ A.delete x

deleteBy :: forall a. (a -> a -> Boolean) -> a -> NonEmptyArray a -> Array a
deleteBy f x = adaptAny $ A.deleteBy f x

difference :: forall a. Eq a => NonEmptyArray a -> NonEmptyArray a -> Array a
difference xs = adaptAny $ difference' xs

difference' :: forall a. Eq a => NonEmptyArray a -> Array a -> Array a
difference' xs = A.difference $ toArray xs

intersect :: forall a . Eq a => NonEmptyArray a -> NonEmptyArray a -> Array a
intersect = intersectBy eq

intersect' :: forall a . Eq a => NonEmptyArray a -> Array a -> Array a
intersect' = intersectBy' eq

intersectBy
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArray a
  -> NonEmptyArray a
  -> Array a
intersectBy eq xs = intersectBy' eq xs <<< toArray

intersectBy'
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArray a
  -> Array a
  -> Array a
intersectBy' eq xs = A.intersectBy eq (toArray xs)

infix 5 difference as \\

zipWith
  :: forall a b c
   . (a -> b -> c)
  -> NonEmptyArray a
  -> NonEmptyArray b
  -> NonEmptyArray c
zipWith f xs ys = unsafeFromArray $ A.zipWith f (toArray xs) (toArray ys)


zipWithA
  :: forall m a b c
   . Applicative m
  => (a -> b -> m c)
  -> NonEmptyArray a
  -> NonEmptyArray b
  -> m (NonEmptyArray c)
zipWithA f xs ys = unsafeFromArrayF $ A.zipWithA f (toArray xs) (toArray ys)

zip :: forall a b. NonEmptyArray a -> NonEmptyArray b -> NonEmptyArray (Tuple a b)
zip xs ys = unsafeFromArray $ toArray xs `A.zip` toArray ys

unzip :: forall a b. NonEmptyArray (Tuple a b) -> Tuple (NonEmptyArray a) (NonEmptyArray b)
unzip = bimap unsafeFromArray unsafeFromArray <<< A.unzip <<< toArray

any :: forall a. (a -> Boolean) -> NonEmptyArray a -> Boolean
any p = adaptAny $ A.any p

all :: forall a. (a -> Boolean) -> NonEmptyArray a -> Boolean
all p = adaptAny $ A.all p

foldM :: forall m a b. Monad m => (b -> a -> m b) -> b -> NonEmptyArray a -> m b
foldM f acc = adaptAny $ A.foldM f acc

foldRecM :: forall m a b. MonadRec m => (b -> a -> m b) -> b -> NonEmptyArray a -> m b
foldRecM f acc = adaptAny $ A.foldRecM f acc

unsafeIndex :: forall a. Partial => NonEmptyArray a -> Int -> a
unsafeIndex = adaptAny A.unsafeIndex
