-- | This module defines a strict catenable list.
-- |
-- | The implementation is based on a queue where all operations require
-- | `O(1)` amortized time.
-- |
-- | However, any single `uncons` operation may run in `O(n)` time.
-- |
-- | See [Purely Functional Data Structures](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf) (Okasaki 1996)
module Data.CatList
  ( CatList(..)
  , empty
  , null
  , singleton
  , length
  , append
  , cons
  , snoc
  , uncons
  , fromFoldable
  ) where

import Prelude hiding (append)

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.CatQueue as Q
import Data.Foldable (class Foldable, foldMapDefaultL)
import Data.Foldable as Foldable
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse, class Traversable)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1)

-- | A strict catenable list.
-- |
-- | `CatList` may be empty, represented by `CatNil`.
-- |
-- | `CatList` may be non-empty, represented by `CatCons`. The `CatCons`
-- | data constructor takes the first element of the list and a queue of
-- | `CatList`.
data CatList a = CatNil | CatCons a (Q.CatQueue (CatList a))

-- | Create an empty catenable list.
-- |
-- | Running time: `O(1)`
empty :: forall a. CatList a
empty = CatNil

-- | Test whether a catenable list is empty.
-- |
-- | Running time: `O(1)`
null :: forall a. CatList a -> Boolean
null CatNil = true
null _ = false

-- | Number of elements in queue.
-- |
-- | Running time: `O(n)` in length of queue.
length :: forall a. CatList a -> Int
length = Foldable.length

-- | Append all elements of a catenable list to the end of another
-- | catenable list, create a new catenable list.
-- |
-- | Running time: `O(1)`
append :: forall a. CatList a -> CatList a -> CatList a
append = link

-- | Append an element to the beginning of the catenable list, creating a new
-- | catenable list.
-- |
-- | Running time: `O(1)`
cons :: forall a. a -> CatList a -> CatList a
cons a cat = append (CatCons a Q.empty) cat

-- | Create a catenable list with a single item.
-- |
-- | Running time: `O(1)`
singleton :: forall a. a -> CatList a
singleton a = cons a CatNil

-- | Append an element to the end of the catenable list, creating a new
-- | catenable list.
-- |
-- | Running time: `O(1)`
snoc :: forall a. CatList a -> a -> CatList a
snoc cat a = append cat (CatCons a Q.empty)

-- | Decompose a catenable list into a `Tuple` of the first element and
-- | the rest of the catenable list.
-- |
-- | Running time: `O(1)`
-- |
-- | Note that any single operation may run in `O(n)`.
uncons :: forall a. CatList a -> Maybe (Tuple a (CatList a))
uncons CatNil = Nothing
uncons (CatCons a q) = Just (Tuple a (if Q.null q then CatNil else (foldr link CatNil q)))

-- | Links two catenable lists by making appending the queue in the
-- | first catenable list to the second catenable list. This operation
-- | creates a new catenable list.
-- |
-- | Running time: `O(1)`
link :: forall a. CatList a -> CatList a -> CatList a
link CatNil cat = cat
link cat CatNil = cat
link (CatCons a q) cat = CatCons a (Q.snoc q cat)

-- | Tail recursive version of foldr on `CatList`.
-- |
-- | Ensures foldl on `List` is tail-recursive.
foldr :: forall a. (CatList a -> CatList a -> CatList a) -> CatList a -> Q.CatQueue (CatList a) -> CatList a
foldr k b q = go q L.Nil
  where
  go :: Q.CatQueue (CatList a) -> L.List (CatList a -> CatList a) -> CatList a
  go xs ys = case Q.uncons xs of
                  Nothing -> foldl (\x i -> i x) b ys
                  Just (Tuple a rest) -> go rest (L.Cons (k a) ys)

  foldl :: forall b c. (c -> b -> c) -> c -> L.List b -> c
  foldl _ c L.Nil = c
  foldl k' c (L.Cons b' as) = foldl k' (k' c b') as

-- | Convert any `Foldable` into a `CatList`.
-- |
-- | Running time: `O(n)`
fromFoldable :: forall f. Foldable f => f ~> CatList
fromFoldable f = Foldable.foldMap singleton f

foldMap :: forall a m. Monoid m => (a -> m) -> CatList a -> m
foldMap _ CatNil = mempty
foldMap f (CatCons a q) =
  let d = if Q.null q then CatNil else (foldr link CatNil q)
  in f a <> foldMap f d

-- | Running time: `O(1)`
instance semigroupCatList :: Semigroup (CatList a) where
  append = append

instance monoidCatList :: Monoid (CatList a) where
  mempty = CatNil

instance showCatList :: Show a => Show (CatList a) where
  show CatNil = "CatNil"
  show (CatCons a as) = "(CatList " <> show a <> " " <> show as <> ")"

instance foldableCatList :: Foldable CatList where
  foldMap = foldMapDefaultL
  foldr f s l = Foldable.foldrDefault f s l
  foldl f = go
    where
    go acc q = case uncons q of
       Just (Tuple x xs) -> go (f acc x) xs
       Nothing -> acc

instance unfoldableCatList :: Unfoldable CatList where
  unfoldr f b = go b CatNil
    where
      go source memo = case f source of
        Nothing -> memo
        Just (Tuple one rest) -> go rest (snoc memo one)

instance unfoldable1CatList :: Unfoldable1 CatList where
  unfoldr1 f b = go b CatNil
    where
      go source memo = case f source of
        Tuple one Nothing -> snoc memo one
        Tuple one (Just rest) -> go rest (snoc memo one)

instance traversableCatList :: Traversable CatList where
  traverse _ CatNil = pure CatNil
  traverse f (CatCons a q) =
    let d = if Q.null q then CatNil else (foldr link CatNil q)
    in cons <$> f a <*> traverse f d
  sequence CatNil = pure CatNil
  sequence (CatCons a q) =
    let d = if Q.null q then CatNil else (foldr link CatNil q)
    in cons <$> a <*> sequence d

instance functorCatList :: Functor CatList where
  map _ CatNil = CatNil
  map f (CatCons a q) =
    let d = if Q.null q then CatNil else (foldr link CatNil q)
    in f a `cons` map f d

instance applyCatList :: Apply CatList where
  apply = ap

instance applicativeCatList :: Applicative CatList where
  pure = singleton

instance bindCatList :: Bind CatList where
  bind = flip foldMap

instance monadCatList :: Monad CatList

instance altCatList :: Alt CatList where
  alt = append

instance plusCatList :: Plus CatList where
  empty = empty

instance alternativeCatList :: Alternative CatList

instance monadPlusCatList :: MonadPlus CatList
