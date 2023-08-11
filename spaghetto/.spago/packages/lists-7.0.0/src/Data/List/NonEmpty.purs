module Data.List.NonEmpty
  ( module Data.List.Types
  , toUnfoldable
  , fromFoldable
  , fromList
  , toList
  , singleton
  , length
  , cons
  , cons'
  , snoc
  , snoc'
  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc
  , (!!), index
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , insertAt
  , updateAt
  , modifyAt
  , reverse
  , concat
  , concatMap
  , filter
  , filterM
  , mapMaybe
  , catMaybes
  , appendFoldable
  , sort
  , sortBy
  , take
  , takeWhile
  , drop
  , dropWhile
  , span
  , group
  , groupAll
  , groupBy
  , groupAllBy
  , partition
  , nub
  , nubBy
  , nubEq
  , nubByEq
  , union
  , unionBy
  , intersect
  , intersectBy
  , zipWith
  , zipWithA
  , zip
  , unzip
  , foldM
  , module Exports
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.List ((:))
import Data.List as L
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.NonEmpty as NE
import Data.Semigroup.Traversable (sequence1)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Partial.Unsafe (unsafeCrashWith)

import Data.Foldable (foldl, foldr, foldMap, fold, intercalate, elem, notElem, find, findMap, any, all) as Exports
import Data.Semigroup.Foldable (fold1, foldMap1, for1_, sequence1_, traverse1_) as Exports
import Data.Semigroup.Traversable (sequence1, traverse1, traverse1Default) as Exports
import Data.Traversable (scanl, scanr) as Exports

-- | Internal function: any operation on a list that is guaranteed not to delete
-- | all elements also applies to a NEL, this function is a helper for defining
-- | those cases.
wrappedOperation
  :: forall a b
   . String
  -> (L.List a -> L.List b)
  -> NonEmptyList a
  -> NonEmptyList b
wrappedOperation name f (NonEmptyList (x :| xs)) =
  case f (x : xs) of
    x' : xs' -> NonEmptyList (x' :| xs')
    L.Nil -> unsafeCrashWith ("Impossible: empty list in NonEmptyList " <> name)

-- | Like `wrappedOperation`, but for functions that operate on 2 lists.
wrappedOperation2
  :: forall a b c
   . String
  -> (L.List a -> L.List b -> L.List c)
  -> NonEmptyList a
  -> NonEmptyList b
  -> NonEmptyList c
wrappedOperation2 name f (NonEmptyList (x :| xs)) (NonEmptyList (y :| ys)) =
  case f (x : xs) (y : ys) of
    x' : xs' -> NonEmptyList (x' :| xs')
    L.Nil -> unsafeCrashWith ("Impossible: empty list in NonEmptyList " <> name)

-- | Lifts a function that operates on a list to work on a NEL. This does not
-- | preserve the non-empty status of the result.
lift :: forall a b. (L.List a -> b) -> NonEmptyList a -> b
lift f (NonEmptyList (x :| xs)) = f (x : xs)

toUnfoldable :: forall f. Unfoldable f => NonEmptyList ~> f
toUnfoldable =
  unfoldr (\xs -> (\rec -> Tuple rec.head rec.tail) <$> L.uncons xs) <<< toList

fromFoldable :: forall f a. Foldable f => f a -> Maybe (NonEmptyList a)
fromFoldable = fromList <<< L.fromFoldable

fromList :: forall a. L.List a -> Maybe (NonEmptyList a)
fromList L.Nil = Nothing
fromList (x : xs) = Just (NonEmptyList (x :| xs))

toList :: NonEmptyList ~> L.List
toList (NonEmptyList (x :| xs)) = x : xs

singleton :: forall a. a -> NonEmptyList a
singleton = NonEmptyList <<< NE.singleton

cons :: forall a. a -> NonEmptyList a -> NonEmptyList a
cons y (NonEmptyList (x :| xs)) = NonEmptyList (y :| x : xs)

cons' :: forall a. a -> L.List a -> NonEmptyList a
cons' x xs = NonEmptyList (x :| xs)

snoc :: forall a. NonEmptyList a -> a -> NonEmptyList a
snoc (NonEmptyList (x :| xs)) y = NonEmptyList (x :| L.snoc xs y)

snoc' :: forall a. L.List a -> a -> NonEmptyList a
snoc' (x : xs) y = NonEmptyList (x :| L.snoc xs y)
snoc' L.Nil y = singleton y

head :: forall a. NonEmptyList a -> a
head (NonEmptyList (x :| _)) = x

last :: forall a. NonEmptyList a -> a
last (NonEmptyList (x :| xs)) = fromMaybe x (L.last xs)

tail :: NonEmptyList ~> L.List
tail (NonEmptyList (_ :| xs)) = xs

init :: NonEmptyList ~> L.List
init (NonEmptyList (x :| xs)) = maybe L.Nil (x : _) (L.init xs)

uncons :: forall a. NonEmptyList a -> { head :: a, tail :: L.List a }
uncons (NonEmptyList (x :| xs)) = { head: x, tail: xs }

unsnoc :: forall a. NonEmptyList a -> { init :: L.List a, last :: a }
unsnoc (NonEmptyList (x :| xs)) = case L.unsnoc xs of
  Nothing -> { init: L.Nil, last: x }
  Just un -> { init: x : un.init, last: un.last }

length :: forall a. NonEmptyList a -> Int
length (NonEmptyList (_ :| xs)) = 1 + L.length xs

index :: forall a. NonEmptyList a -> Int -> Maybe a
index (NonEmptyList (x :| xs)) i
  | i == 0 = Just x
  | otherwise = L.index xs (i - 1)

infixl 8 index as !!

elemIndex :: forall a. Eq a => a -> NonEmptyList a -> Maybe Int
elemIndex x = findIndex (_ == x)

elemLastIndex :: forall a. Eq a => a -> NonEmptyList a -> Maybe Int
elemLastIndex x = findLastIndex (_ == x)

findIndex :: forall a. (a -> Boolean) -> NonEmptyList a -> Maybe Int
findIndex f (NonEmptyList (x :| xs))
  | f x = Just 0
  | otherwise = (_ + 1) <$> L.findIndex f xs

findLastIndex :: forall a. (a -> Boolean) -> NonEmptyList a -> Maybe Int
findLastIndex f (NonEmptyList (x :| xs)) =
  case L.findLastIndex f xs of
    Just i -> Just (i + 1)
    Nothing
      | f x -> Just 0
      | otherwise -> Nothing

insertAt :: forall a. Int -> a -> NonEmptyList a -> Maybe (NonEmptyList a)
insertAt i a (NonEmptyList (x :| xs))
  | i == 0 = Just (NonEmptyList (a :| x : xs))
  | otherwise = NonEmptyList <<< (x :| _) <$> L.insertAt (i - 1) a xs

updateAt :: forall a. Int -> a -> NonEmptyList a -> Maybe (NonEmptyList a)
updateAt i a (NonEmptyList (x :| xs))
  | i == 0 = Just (NonEmptyList (a :| xs))
  | otherwise = NonEmptyList <<< (x :| _) <$> L.updateAt (i - 1) a xs

modifyAt :: forall a. Int -> (a -> a) -> NonEmptyList a -> Maybe (NonEmptyList a)
modifyAt i f (NonEmptyList (x :| xs))
  | i == 0 = Just (NonEmptyList (f x :| xs))
  | otherwise = NonEmptyList <<< (x :| _) <$> L.modifyAt (i - 1) f xs

reverse :: forall a. NonEmptyList a -> NonEmptyList a
reverse = wrappedOperation "reverse" L.reverse

filter :: forall a. (a -> Boolean) -> NonEmptyList a -> L.List a
filter = lift <<< L.filter

filterM :: forall m a. Monad m => (a -> m Boolean) -> NonEmptyList a -> m (L.List a)
filterM = lift <<< L.filterM

mapMaybe :: forall a b. (a -> Maybe b) -> NonEmptyList a -> L.List b
mapMaybe = lift <<< L.mapMaybe

catMaybes :: forall a. NonEmptyList (Maybe a) -> L.List a
catMaybes = lift L.catMaybes

concat :: forall a. NonEmptyList (NonEmptyList a) -> NonEmptyList a
concat = (_ >>= identity)

concatMap :: forall a b. (a -> NonEmptyList b) -> NonEmptyList a -> NonEmptyList b
concatMap = flip bind

appendFoldable :: forall t a. Foldable t => NonEmptyList a -> t a -> NonEmptyList a
appendFoldable (NonEmptyList (x :| xs)) ys =
  NonEmptyList (x :| (xs <> L.fromFoldable ys))

sort :: forall a. Ord a => NonEmptyList a -> NonEmptyList a
sort xs = sortBy compare xs

sortBy :: forall a. (a -> a -> Ordering) -> NonEmptyList a -> NonEmptyList a
sortBy = wrappedOperation "sortBy" <<< L.sortBy

take :: forall a. Int -> NonEmptyList a -> L.List a
take = lift <<< L.take

takeWhile :: forall a. (a -> Boolean) -> NonEmptyList a -> L.List a
takeWhile = lift <<< L.takeWhile

drop :: forall a. Int -> NonEmptyList a -> L.List a
drop = lift <<< L.drop

dropWhile :: forall a. (a -> Boolean) -> NonEmptyList a -> L.List a
dropWhile = lift <<< L.dropWhile

span :: forall a. (a -> Boolean) -> NonEmptyList a -> { init :: L.List a, rest :: L.List a }
span = lift <<< L.span

group :: forall a. Eq a => NonEmptyList a -> NonEmptyList (NonEmptyList a)
group = wrappedOperation "group" L.group

groupAll :: forall a. Ord a => NonEmptyList a -> NonEmptyList (NonEmptyList a)
groupAll = wrappedOperation "groupAll" L.groupAll

groupBy :: forall a. (a -> a -> Boolean) -> NonEmptyList a -> NonEmptyList (NonEmptyList a)
groupBy = wrappedOperation "groupBy" <<< L.groupBy

groupAllBy :: forall a. (a -> a -> Ordering) -> NonEmptyList a -> NonEmptyList (NonEmptyList a)
groupAllBy = wrappedOperation "groupAllBy" <<< L.groupAllBy

partition :: forall a. (a -> Boolean) -> NonEmptyList a -> { yes :: L.List a, no :: L.List a }
partition = lift <<< L.partition

nub :: forall a. Ord a => NonEmptyList a -> NonEmptyList a
nub = wrappedOperation "nub" L.nub

nubBy :: forall a. (a -> a -> Ordering) -> NonEmptyList a -> NonEmptyList a
nubBy = wrappedOperation "nubBy" <<< L.nubBy

nubEq :: forall a. Eq a => NonEmptyList a -> NonEmptyList a
nubEq = wrappedOperation "nubEq" L.nubEq

nubByEq :: forall a. (a -> a -> Boolean) -> NonEmptyList a -> NonEmptyList a
nubByEq = wrappedOperation "nubByEq" <<< L.nubByEq

union :: forall a. Eq a => NonEmptyList a -> NonEmptyList a -> NonEmptyList a
union = wrappedOperation2 "union" L.union

unionBy :: forall a. (a -> a -> Boolean) -> NonEmptyList a -> NonEmptyList a -> NonEmptyList a
unionBy = wrappedOperation2 "unionBy" <<< L.unionBy

intersect :: forall a. Eq a => NonEmptyList a -> NonEmptyList a -> NonEmptyList a
intersect = wrappedOperation2 "intersect" L.intersect

intersectBy :: forall a. (a -> a -> Boolean) -> NonEmptyList a -> NonEmptyList a -> NonEmptyList a
intersectBy = wrappedOperation2 "intersectBy" <<< L.intersectBy

zipWith :: forall a b c. (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
zipWith f (NonEmptyList (x :| xs)) (NonEmptyList (y :| ys)) =
  NonEmptyList (f x y :| L.zipWith f xs ys)

zipWithA :: forall m a b c. Applicative m => (a -> b -> m c) -> NonEmptyList a -> NonEmptyList b -> m (NonEmptyList c)
zipWithA f xs ys = sequence1 (zipWith f xs ys)

zip :: forall a b. NonEmptyList a -> NonEmptyList b -> NonEmptyList (Tuple a b)
zip = zipWith Tuple

unzip :: forall a b. NonEmptyList (Tuple a b) -> Tuple (NonEmptyList a) (NonEmptyList b)
unzip ts = Tuple (map fst ts) (map snd ts)

foldM :: forall m a b. Monad m => (b -> a -> m b) -> b -> NonEmptyList a -> m b
foldM f b (NonEmptyList (a :| as)) = f b a >>= \b' -> L.foldM f b' as
