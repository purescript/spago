-- | This module defines a type of _lazy_ linked lists, and associated helper
-- | functions and type class instances.
-- |
-- | _Note_: Depending on your use-case, you may prefer to use
-- | `Data.Sequence` instead, which might give better performance for certain
-- | use cases. This module is an improvement over `Data.Array` when working with
-- | immutable lists of data in a purely-functional setting, but does not have
-- | good random-access performance.

module Data.List.Lazy
  ( module Data.List.Lazy.Types
  , toUnfoldable
  , fromFoldable

  , singleton
  , (..), range
  , replicate
  , replicateM
  , some
  , many
  , repeat
  , iterate
  , cycle

  , null
  , length

  , snoc
  , insert
  , insertBy

  , head
  , last
  , tail
  , init
  , uncons

  , (!!), index
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , modifyAt
  , alterAt

  , reverse
  , concat
  , concatMap
  , filter
  , filterM
  , mapMaybe
  , catMaybes

  -- , sort
  -- , sortBy

  , Pattern(..)
  , stripPrefix
  , slice
  , take
  , takeWhile
  , drop
  , dropWhile
  , span
  , group
  -- , group'
  , groupBy
  , partition

  , nub
  , nubBy
  , nubEq
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

  , transpose

  , foldM
  , foldrLazy
  , scanlLazy

  , module Exports
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Lazy as Z
import Control.Monad.Rec.Class as Rec
import Data.Foldable (class Foldable, foldr, any, foldl)
import Data.Foldable (foldl, foldr, foldMap, fold, intercalate, elem, notElem, find, findMap, any, all) as Exports
import Data.Lazy (defer)
import Data.List.Internal (emptySet, insertAndLookupBy)
import Data.List.Lazy.Types (List(..), Step(..), step, nil, cons, (:))
import Data.List.Lazy.Types (NonEmptyList(..)) as NEL
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Traversable (scanl, scanr) as Exports
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

-- | Convert a list into any unfoldable structure.
-- |
-- | Running time: `O(n)`
toUnfoldable :: forall f. Unfoldable f => List ~> f
toUnfoldable = unfoldr (\xs -> (\rec -> Tuple rec.head rec.tail) <$> uncons xs)

-- | Construct a list from a foldable structure.
-- |
-- | Running time: `O(n)`
fromFoldable :: forall f. Foldable f => f ~> List
fromFoldable = foldr cons nil

fromStep :: forall a. Step a -> List a
fromStep = List <<< pure

--------------------------------------------------------------------------------
-- List creation ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Create a list with a single element.
-- |
-- | Running time: `O(1)`
singleton :: forall a. a -> List a
singleton a = cons a nil

-- | An infix synonym for `range`.
infix 8 range as ..

-- | Create a list containing a range of integers, including both endpoints.
range :: Int -> Int -> List Int
range start end
    | start > end =
        let g x | x >= end  = Just (Tuple x (x - 1))
                | otherwise = Nothing
         in unfoldr g start
    | otherwise = unfoldr f start
  where
    f x | x <= end  = Just (Tuple x (x + 1))
        | otherwise = Nothing

-- | Create a list with repeated instances of a value.
replicate :: forall a. Int -> a -> List a
replicate i xs = take i (repeat xs)

-- | Perform a monadic action `n` times collecting all of the results.
replicateM :: forall m a. Monad m => Int -> m a -> m (List a)
replicateM n m
  | n < one = pure nil
  | otherwise = do
      a <- m
      as <- replicateM (n - one) m
      pure (cons a as)

-- | Attempt a computation multiple times, requiring at least one success.
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
some :: forall f a. Alternative f => Z.Lazy (f (List  a)) => f a -> f (List  a)
some v = cons <$> v <*> Z.defer (\_ -> many v)

-- | Attempt a computation multiple times, returning as many successful results
-- | as possible (possibly zero).
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
many :: forall f a. Alternative f => Z.Lazy (f (List a)) => f a -> f (List a)
many v = some v <|> pure nil

-- | Create a list by repeating an element
repeat :: forall a. a -> List a
repeat x = Z.fix \xs -> cons x xs

-- | Create a list by iterating a function
iterate :: forall a. (a -> a) -> a -> List a
iterate f x = Z.fix \xs -> cons x (f <$> xs)

-- | Create a list by repeating another list
cycle :: forall a. List a -> List a
cycle xs = Z.fix \ys -> xs <> ys

--------------------------------------------------------------------------------
-- List size -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether a list is empty.
-- |
-- | Running time: `O(1)`
null :: forall a. List a -> Boolean
null = isNothing <<< uncons

-- | Get the length of a list
-- |
-- | Running time: `O(n)`
length :: forall a. List a -> Int
length = foldl (\l _ -> l + 1) 0

--------------------------------------------------------------------------------
-- Extending lists -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Append an element to the end of a list, creating a new list.
-- |
-- | Running time: `O(n)`
snoc :: forall a. List a -> a -> List a
snoc xs x = foldr cons (cons x nil) xs

-- | Insert an element into a sorted list.
-- |
-- | Running time: `O(n)`
insert :: forall a. Ord a => a -> List a -> List a
insert = insertBy compare

-- | Insert an element into a sorted list, using the specified function to determine the ordering
-- | of elements.
-- |
-- | Running time: `O(n)`
insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
insertBy cmp x xs = List (go <$> unwrap xs)
  where
  go Nil = Cons x nil
  go ys@(Cons y ys') =
    case cmp x y of
      GT -> Cons y (insertBy cmp x ys')
      _  -> Cons x (fromStep ys)

--------------------------------------------------------------------------------
-- Non-indexed reads -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the first element in a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`.
head :: List ~> Maybe
head xs = _.head <$> uncons xs

-- | Get the last element in a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(n)`.
last :: List ~> Maybe
last = go <<< step
  where
  go (Cons x xs)
    | null xs = Just x
    | otherwise = go (step xs)
  go _ = Nothing

-- | Get all but the first element of a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`
tail :: forall a. List a -> Maybe (List a)
tail xs = _.tail <$> uncons xs

-- | Get all but the last element of a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(n)`
init :: forall a. List a -> Maybe (List a)
init = go <<< step
  where
  go :: Step a -> Maybe (List a)
  go (Cons x xs)
    | null xs = Just nil
    | otherwise = cons x <$> go (step xs)
  go _ = Nothing

-- | Break a list into its first element, and the remaining elements,
-- | or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons xs = case step xs of
  Nil -> Nothing
  Cons x xs' -> Just { head: x, tail: xs' }

--------------------------------------------------------------------------------
-- Indexed operations ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the element at the specified index, or `Nothing` if the index is out-of-bounds.
-- |
-- | Running time: `O(n)` where `n` is the required index.
index :: forall a. List a -> Int -> Maybe a
index xs = go (step xs)
  where
  go Nil _ = Nothing
  go (Cons a _) 0 = Just a
  go (Cons _ as) i = go (step as) (i - 1)

-- | An infix synonym for `index`.
infixl 8 index as !!

-- | Find the index of the first element equal to the specified element.
elemIndex :: forall a. Eq a => a -> List a -> Maybe Int
elemIndex x = findIndex (_ == x)

-- | Find the index of the last element equal to the specified element.
elemLastIndex :: forall a. Eq a => a -> List a -> Maybe Int
elemLastIndex x = findLastIndex (_ == x)

-- | Find the first index for which a predicate holds.
findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex fn = go 0
  where
  go :: Int -> List a -> Maybe Int
  go n list = do
      o <- uncons list
      if fn o.head
         then pure n
         else go (n + 1) o.tail

-- | Find the last index for which a predicate holds.
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex fn xs = ((length xs - 1) - _) <$> findIndex fn (reverse xs)

-- | Insert an element into a list at the specified index, or append the element
-- | to the end of the list if the index is out-of-bounds, returning a new list.
-- |
-- | Running time: `O(n)`
insertAt :: forall a. Int -> a -> List a -> List a
insertAt 0 x xs = cons x xs
insertAt n x xs = List (go <$> unwrap xs)
  where
  go Nil = Cons x nil
  go (Cons y ys) = Cons y (insertAt (n - 1) x ys)

-- | Delete an element from a list at the specified index, returning a new list,
-- | or return the original list unchanged if the index is out-of-bounds.
-- |
-- | Running time: `O(n)`
deleteAt :: forall a. Int -> List a -> List a
deleteAt n xs = List (go n <$> unwrap xs)
  where
  go _ Nil = Nil
  go 0 (Cons _ ys) = step ys
  go n' (Cons y ys) = Cons y (deleteAt (n' - 1) ys)

-- | Update the element at the specified index, returning a new list,
-- | or return the original list unchanged if the index is out-of-bounds.
-- |
-- | Running time: `O(n)`
updateAt :: forall a. Int -> a -> List a -> List a
updateAt n x xs = List (go n <$> unwrap xs)
  where
  go _ Nil = Nil
  go 0 (Cons _ ys) = Cons x ys
  go n' (Cons y ys) = Cons y (updateAt (n' - 1) x ys)

-- | Update the element at the specified index by applying a function to
-- | the current value, returning a new list, or return the original list unchanged
-- | if the index is out-of-bounds.
-- |
-- | Running time: `O(n)`
modifyAt :: forall a. Int -> (a -> a) -> List a -> List a
modifyAt n f = alterAt n (Just <<< f)

-- | Update or delete the element at the specified index by applying a
-- | function to the current value, returning a new list, or return the
-- | original list unchanged if the index is out-of-bounds.
-- |
-- | Running time: `O(n)`
alterAt :: forall a. Int -> (a -> Maybe a) -> List a -> List a
alterAt n f xs = List (go n <$> unwrap xs)
  where
  go _ Nil = Nil
  go 0 (Cons y ys) = case f y of
    Nothing -> step ys
    Just y' -> Cons y' ys
  go n' (Cons y ys) = Cons y (alterAt (n' - 1) f ys)

--------------------------------------------------------------------------------
-- Transformations -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Reverse a list.
-- |
-- | Running time: `O(n)`
reverse :: List ~> List
reverse xs = Z.defer \_ -> foldl (flip cons) nil xs

-- | Flatten a list of lists.
-- |
-- | Running time: `O(n)`, where `n` is the total number of elements.
concat :: forall a. List (List a) -> List a
concat = (_ >>= identity)

-- | Apply a function to each element in a list, and flatten the results
-- | into a single, new list.
-- |
-- | Running time: `O(n)`, where `n` is the total number of elements.
concatMap :: forall a b. (a -> List b) -> List a -> List b
concatMap = flip bind

-- | Filter a list, keeping the elements which satisfy a predicate function.
-- |
-- | Running time: `O(n)`
filter :: forall a. (a -> Boolean) -> List a -> List a
filter p = List <<< map go <<< unwrap
  where
  go Nil = Nil
  go (Cons x xs)
    | p x = Cons x (filter p xs)
    | otherwise = go (step xs)

-- | Filter where the predicate returns a monadic `Boolean`.
-- |
-- | For example:
-- |
-- | ```purescript
-- | powerSet :: forall a. [a] -> [[a]]
-- | powerSet = filterM (const [true, false])
-- | ```
filterM :: forall a m. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM p list =
    case uncons list of
         Nothing -> pure nil
         Just { head: x, tail: xs } -> do
             b <- p x
             xs' <- filterM p xs
             pure if b then cons x xs' else xs'


-- | Apply a function to each element in a list, keeping only the results which
-- | contain a value.
-- |
-- | Running time: `O(n)`
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
mapMaybe f = List <<< map go <<< unwrap
  where
  go Nil = Nil
  go (Cons x xs) =
    case f x of
      Nothing -> go (step xs)
      Just y -> Cons y (mapMaybe f xs)

-- | Filter a list of optional values, keeping only the elements which contain
-- | a value.
catMaybes :: forall a. List (Maybe a) -> List a
catMaybes = mapMaybe identity

--------------------------------------------------------------------------------
-- Sorting ---------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Sublists --------------------------------------------------------------------
--------------------------------------------------------------------------------


-- | A newtype used in cases where there is a list to be matched.
newtype Pattern a = Pattern (List a)

derive instance eqPattern :: Eq a => Eq (Pattern a)
derive instance ordPattern :: Ord a => Ord (Pattern a)
derive instance newtypePattern :: Newtype (Pattern a) _

instance showPattern :: Show a => Show (Pattern a) where
  show (Pattern s) = "(Pattern " <> show s <> ")"


-- | If the list starts with the given prefix, return the portion of the
-- | list left after removing it, as a Just value. Otherwise, return Nothing.
-- | * `stripPrefix (Pattern (fromFoldable [1])) (fromFoldable [1,2]) == Just (fromFoldable [2])`
-- | * `stripPrefix (Pattern (fromFoldable [])) (fromFoldable [1]) == Just (fromFoldable [1])`
-- | * `stripPrefix (Pattern (fromFoldable [2])) (fromFoldable [1]) == Nothing`
-- |
-- | Running time: `O(n)` where `n` is the number of elements to strip.
stripPrefix :: forall a. Eq a => Pattern a -> List a -> Maybe (List a)
stripPrefix (Pattern p') s = Rec.tailRecM2 go p' s
  where
  go prefix input = case step prefix of
    Nil -> Just $ Rec.Done input
    Cons p ps -> case step input of
      Cons i is | p == i -> Just $ Rec.Loop { a: ps, b: is }
      _ -> Nothing

-- | Extract a sublist by a start and end index.
slice :: Int -> Int -> List ~> List
slice start end xs = take (end - start) (drop start xs)

-- | Take the specified number of elements from the front of a list.
-- |
-- | Running time: `O(n)` where `n` is the number of elements to take.
take :: forall a. Int -> List a -> List a
take n = if n <= 0
  then const nil
  else List <<< map (go n) <<< unwrap
  where
  go :: Int -> Step a -> Step a
  go _ Nil = Nil
  go n' (Cons x xs) = Cons x (take (n' - 1) xs)

-- | Take those elements from the front of a list which match a predicate.
-- |
-- | Running time (worst case): `O(n)`
takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile p = List <<< map go <<< unwrap
  where
  go (Cons x xs) | p x = Cons x (takeWhile p xs)
  go _ = Nil

-- | Drop the specified number of elements from the front of a list.
-- |
-- | Running time: `O(n)` where `n` is the number of elements to drop.
drop :: forall a. Int -> List a -> List a
drop n = List <<< map (go n) <<< unwrap
  where
  go 0 xs = xs
  go _ Nil = Nil
  go n' (Cons _ xs) = go (n' - 1) (step xs)

-- | Drop those elements from the front of a list which match a predicate.
-- |
-- | Running time (worst case): `O(n)`
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile p = go <<< step
  where
  go (Cons x xs) | p x = go (step xs)
  go xs = fromStep xs

-- | Split a list into two parts:
-- |
-- | 1. the longest initial segment for which all elements satisfy the specified predicate
-- | 2. the remaining elements
-- |
-- | For example,
-- |
-- | ```purescript
-- | span (\n -> n % 2 == 1) (1 : 3 : 2 : 4 : 5 : Nil) == Tuple (1 : 3 : Nil) (2 : 4 : 5 : Nil)
-- | ```
-- |
-- | Running time: `O(n)`
span :: forall a. (a -> Boolean) -> List a -> { init :: List a, rest :: List a }
span p xs =
  case uncons xs of
    Just { head: x, tail: xs' } | p x ->
      case span p xs' of
        { init: ys, rest: zs } -> { init: cons x ys, rest: zs }
    _ -> { init: nil, rest: xs }

-- | Group equal, consecutive elements of a list into lists.
-- |
-- | For example,
-- |
-- | ```purescript
-- | group (1 : 1 : 2 : 2 : 1 : Nil) == (1 : 1 : Nil) : (2 : 2 : Nil) : (1 : Nil) : Nil
-- | ```
-- |
-- | Running time: `O(n)`
group :: forall a. Eq a => List a -> List (NEL.NonEmptyList a)
group = groupBy (==)

-- | Group equal, consecutive elements of a list into lists, using the specified
-- | equivalence relation to determine equality.
-- |
-- | Running time: `O(n)`
groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (NEL.NonEmptyList a)
groupBy eq = List <<< map go <<< unwrap
  where
  go Nil = Nil
  go (Cons x xs) =
    case span (eq x) xs of
      { init: ys, rest: zs } ->
        Cons (NEL.NonEmptyList (defer \_ -> x :| ys)) (groupBy eq zs)

-- | Returns a tuple of lists of elements which do
-- | and do not satisfy a predicate, respectively.
-- |
-- | Running time: `O(n)`
partition :: forall a. (a -> Boolean) -> List a -> { yes :: List a, no :: List a }
partition f = foldr go {yes: nil, no: nil}
  where
  go x {yes: ys, no: ns} =
    if f x then {yes: x : ys, no: ns} else {yes: ys, no: x : ns}

--------------------------------------------------------------------------------
-- Set-like operations ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Remove duplicate elements from a list.
-- | Keeps the first occurrence of each element in the input list,
-- | in the same order they appear in the input list.
-- |
-- | Running time: `O(n log n)`
nub :: forall a. Ord a => List a -> List a
nub = nubBy compare

-- | Remove duplicate elements from a list based on the provided comparison function.
-- | Keeps the first occurrence of each element in the input list,
-- | in the same order they appear in the input list.
-- |
-- | Running time: `O(n log n)`
nubBy :: forall a. (a -> a -> Ordering) -> List a -> List a
nubBy p = go emptySet
  where
    go s (List l) = List (map (goStep s) l)
    goStep _ Nil = Nil
    goStep s (Cons a as) =
      let { found, result: s' } = insertAndLookupBy p a s
      in if found
        then step (go s' as)
        else Cons a (go s' as)

-- | Remove duplicate elements from a list.
-- |
-- | Running time: `O(n^2)`
nubEq :: forall a. Eq a => List a -> List a
nubEq = nubByEq eq

-- | Remove duplicate elements from a list, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n^2)`
nubByEq :: forall a. (a -> a -> Boolean) -> List a -> List a
nubByEq eq = List <<< map go <<< unwrap
  where
  go Nil = Nil
  go (Cons x xs) = Cons x (nubByEq eq (filter (\y -> not (eq x y)) xs))

-- | Calculate the union of two lists.
-- |
-- | Running time: `O(n^2)`
union :: forall a. Eq a => List a -> List a -> List a
union = unionBy (==)

-- | Calculate the union of two lists, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n^2)`
unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
unionBy eq xs ys = xs <> foldl (flip (deleteBy eq)) (nubByEq eq ys) xs

-- | Delete the first occurrence of an element from a list.
-- |
-- | Running time: `O(n)`
delete :: forall a. Eq a => a -> List a -> List a
delete = deleteBy (==)

-- | Delete the first occurrence of an element from a list, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n)`
deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
deleteBy eq x xs = List (go <$> unwrap xs)
  where
  go Nil = Nil
  go (Cons y ys) | eq x y = step ys
                 | otherwise = Cons y (deleteBy eq x ys)

-- | Delete the first occurrence of each element in the second list from the first list.
-- |
-- | Running time: `O(n^2)`
difference :: forall a. Eq a => List a -> List a -> List a
difference = foldl (flip delete)
infix 5 difference as \\

-- | Calculate the intersection of two lists.
-- |
-- | Running time: `O(n^2)`
intersect :: forall a. Eq a => List a -> List a -> List a
intersect = intersectBy (==)

-- | Calculate the intersection of two lists, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n^2)`
intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
intersectBy eq xs ys = filter (\x -> any (eq x) ys) xs

--------------------------------------------------------------------------------
-- Zipping ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Apply a function to pairs of elements at the same positions in two lists,
-- | collecting the results in a new list.
-- |
-- | If one list is longer, elements will be discarded from the longer list.
-- |
-- | For example
-- |
-- | ```purescript
-- | zipWith (*) (1 : 2 : 3 : Nil) (4 : 5 : 6 : 7 Nil) == 4 : 10 : 18 : Nil
-- | ```
-- |
-- | Running time: `O(min(m, n))`
zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = List (go <$> unwrap xs <*> unwrap ys)
  where
  go :: Step a -> Step b -> Step c
  go Nil _ = Nil
  go _ Nil = Nil
  go (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)

-- | A generalization of `zipWith` which accumulates results in some `Applicative`
-- | functor.
zipWithA :: forall m a b c. Applicative m => (a -> b -> m c) -> List a -> List b -> m (List c)
zipWithA f xs ys = sequence (zipWith f xs ys)

-- | Collect pairs of elements at the same positions in two lists.
-- |
-- | Running time: `O(min(m, n))`
zip :: forall a b. List a -> List b -> List (Tuple a b)
zip = zipWith Tuple

-- | Transforms a list of pairs into a list of first components and a list of
-- | second components.
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip = foldr (\(Tuple a b) (Tuple as bs) -> Tuple (cons a as) (cons b bs)) (Tuple nil nil)

--------------------------------------------------------------------------------
-- Transpose -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The 'transpose' function transposes the rows and columns of its argument.
-- | For example,
-- |
-- |     transpose ((1:2:3:nil) : (4:5:6:nil) : nil) ==
-- |       ((1:4:nil) : (2:5:nil) : (3:6:nil) : nil)
-- |
-- | If some of the rows are shorter than the following rows, their elements are skipped:
-- |
-- |     transpose ((10:11:nil) : (20:nil) : nil : (30:31:32:nil) : nil) ==
-- |       ((10:20:30:nil) : (11:31:nil) : (32:nil) : nil)
transpose :: forall a. List (List a) -> List (List a)
transpose xs =
  case uncons xs of
    Nothing ->
      xs
    Just { head: h, tail: xss } ->
      case uncons h of
        Nothing ->
          transpose xss
        Just { head: x, tail: xs' } ->
          (x : mapMaybe head xss) : transpose (xs' : mapMaybe tail xss)

--------------------------------------------------------------------------------
-- Folding ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Perform a fold using a monadic step function.
foldM :: forall m a b. Monad m => (b -> a -> m b) -> b -> List a -> m b
foldM f b xs =
    case uncons xs of
         Nothing -> pure b
         Just { head: a, tail: as } ->
                       f b a >>= \b' -> foldM f b' as

-- | Perform a right fold lazily
foldrLazy :: forall a b. Z.Lazy b => (a -> b -> b) -> b -> List a -> b
foldrLazy op z = go
  where
    go xs = case step xs of
      Cons x xs' -> Z.defer \_ -> x `op` go xs'
      Nil -> z

-- | Perform a left scan lazily
scanlLazy :: forall a b. (b -> a -> b) -> b -> List a -> List b
scanlLazy f acc xs = List (go <$> unwrap xs)
  where
    go :: Step a -> Step b
    go Nil = Nil
    go (Cons x xs') =
      let acc' = f acc x
       in Cons acc' $ scanlLazy f acc' xs'
