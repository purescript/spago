-- | This module defines a generic non-empty data structure, which adds an
-- | additional element to any container type.
module Data.NonEmpty
  ( NonEmpty(..)
  , singleton
  , (:|)
  , foldl1
  , fromNonEmpty
  , oneOf
  , head
  , tail
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Plus (class Plus, empty)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord1)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Foldable (foldl1) as Foldable1
import Data.Traversable (class Traversable, traverse, sequence)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (uncurry)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Unfoldable1 (class Unfoldable1)

-- | A non-empty container of elements of type a.
-- |
-- | ```purescript
-- | import Data.NonEmpty
-- |
-- | nonEmptyArray :: NonEmpty Array Int
-- | nonEmptyArray = NonEmpty 1 [2,3]
-- |
-- | import Data.List(List(..), (:))
-- |
-- | nonEmptyList :: NonEmpty List Int
-- | nonEmptyList = NonEmpty 1 (2 : 3 : Nil)
-- | ```
data NonEmpty f a = NonEmpty a (f a)

-- | An infix synonym for `NonEmpty`.
-- |
-- | ```purescript
-- | nonEmptyArray :: NonEmpty Array Int
-- | nonEmptyArray = 1 :| [2,3]
-- |
-- | nonEmptyList :: NonEmpty List Int
-- | nonEmptyList = 1 :| 2 : 3 : Nil
-- | ```
infixr 5 NonEmpty as :|

-- | Create a non-empty structure with a single value.
-- |
-- | ```purescript
-- | import Prelude
-- |
-- | singleton 1 == 1 :| []
-- | singleton 1 == 1 :| Nil
-- | ```
singleton :: forall f a. Plus f => a -> NonEmpty f a
singleton a = a :| empty

-- | Fold a non-empty structure, collecting results using a binary operation.
-- |
-- | ```purescript
-- | foldl1 (+) (1 :| [2, 3]) == 6
-- | ```
foldl1 :: forall f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 = Foldable1.foldl1

-- | Apply a function that takes the `first` element and remaining elements
-- | as arguments to a non-empty container.
-- |
-- | For example, return the remaining elements multiplied by the first element:
-- |
-- | ```purescript
-- | fromNonEmpty (\x xs -> map (_ * x) xs) (3 :| [2, 1]) == [6, 3]
-- | ```
fromNonEmpty :: forall f a r. (a -> f a -> r) -> NonEmpty f a -> r
fromNonEmpty f (a :| fa) = a `f` fa

-- | Returns the `alt` (`<|>`) result of:
-- | - The first element lifted to the container of the remaining elements.
-- | - The remaining elements.
-- |
-- | ```purescript
-- | import Data.Maybe(Maybe(..))
-- |
-- | oneOf (1 :| Nothing) == Just 1
-- | oneOf (1 :| Just 2) == Just 1
-- |
-- | oneOf (1 :| [2, 3]) == [1,2,3]
-- | ```
oneOf :: forall f a. Alternative f => NonEmpty f a -> f a
oneOf (a :| fa) = pure a <|> fa

-- | Get the 'first' element of a non-empty container.
-- |
-- | ```purescript
-- | head (1 :| [2, 3]) == 1
-- | ```
head :: forall f a. NonEmpty f a -> a
head (x :| _) = x

-- | Get everything but the 'first' element of a non-empty container.
-- |
-- | ```purescript
-- | tail (1 :| [2, 3]) == [2, 3]
-- | ```
tail :: forall f a. NonEmpty f a -> f a
tail (_ :| xs) = xs

instance showNonEmpty :: (Show a, Show (f a)) => Show (NonEmpty f a) where
  show (a :| fa) = "(NonEmpty " <> show a <> " " <> show fa <> ")"

derive instance eqNonEmpty :: (Eq1 f, Eq a) => Eq (NonEmpty f a)

derive instance eq1NonEmpty :: Eq1 f => Eq1 (NonEmpty f)

derive instance ordNonEmpty :: (Ord1 f, Ord a) => Ord (NonEmpty f a)

derive instance ord1NonEmpty :: Ord1 f => Ord1 (NonEmpty f)

derive instance functorNonEmpty :: Functor f => Functor (NonEmpty f)

instance functorWithIndex
  :: FunctorWithIndex i f
  => FunctorWithIndex (Maybe i) (NonEmpty f) where
  mapWithIndex f (a :| fa) = f Nothing a :| mapWithIndex (f <<< Just) fa

instance foldableNonEmpty :: Foldable f => Foldable (NonEmpty f) where
  foldMap f (a :| fa) = f a <> foldMap f fa
  foldl f b (a :| fa) = foldl f (f b a) fa
  foldr f b (a :| fa) = f a (foldr f b fa)

instance foldableWithIndexNonEmpty
  :: (FoldableWithIndex i f)
  => FoldableWithIndex (Maybe i) (NonEmpty f) where
  foldMapWithIndex f (a :| fa) = f Nothing a <> foldMapWithIndex (f <<< Just) fa
  foldlWithIndex f b (a :| fa) = foldlWithIndex (f <<< Just) (f Nothing b a) fa
  foldrWithIndex f b (a :| fa) = f Nothing a (foldrWithIndex (f <<< Just) b fa)

instance traversableNonEmpty :: Traversable f => Traversable (NonEmpty f) where
  sequence (a :| fa) = NonEmpty <$> a <*> sequence fa
  traverse f (a :| fa) = NonEmpty <$> f a <*> traverse f fa

instance traversableWithIndexNonEmpty
  :: (TraversableWithIndex i f)
  => TraversableWithIndex (Maybe i) (NonEmpty f) where
  traverseWithIndex f (a :| fa) =
    NonEmpty <$> f Nothing a <*> traverseWithIndex (f <<< Just) fa

instance foldable1NonEmpty :: Foldable f => Foldable1 (NonEmpty f) where
  foldMap1 f (a :| fa) = foldl (\s a1 -> s <> f a1) (f a) fa
  foldr1 f (a :| fa) = maybe a (f a) $ foldr (\a1 -> Just <<< maybe a1 (f a1)) Nothing fa
  foldl1 f (a :| fa) = foldl f a fa

instance unfoldable1NonEmpty :: Unfoldable f => Unfoldable1 (NonEmpty f) where
  unfoldr1 f b = uncurry (:|) $ unfoldr (map f) <$> f b

-- | This is a lawful `Semigroup` instance that will behave sensibly for common nonempty
-- | containers like lists and arrays. However, it's not guaranteed that `pure` will behave
-- | sensibly alongside `<>` for all types, as we don't have any laws which govern their behavior.
instance semigroupNonEmpty
  :: (Applicative f, Semigroup (f a))
  => Semigroup (NonEmpty f a) where
  append (a1 :| f1) (a2 :| f2) = a1 :| (f1 <> pure a2 <> f2)
