module Data.Ord.Max where

import Prelude

import Data.Newtype (class Newtype)

-- | Provides a `Semigroup` based on the `max` function. If the type has a
-- | `Bounded` instance, then a `Monoid` instance is provided too. For example:
-- |
-- |     unwrap (Max 5 <> Max 6) = 6
-- |     mempty :: Max Ordering = Max LT
-- |
newtype Max a = Max a

derive instance newtypeMax :: Newtype (Max a) _

derive newtype instance eqMax :: Eq a => Eq (Max a)

instance ordMax :: Ord a => Ord (Max a) where
  compare (Max x) (Max y) = compare x y

instance semigroupMax :: Ord a => Semigroup (Max a) where
  append (Max x) (Max y) = Max (max x y)

instance monoidMax :: Bounded a => Monoid (Max a) where
  mempty = Max bottom

instance showMax :: Show a => Show (Max a) where
  show (Max a) = "(Max " <> show a <> ")"
