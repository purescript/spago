module Data.Ord.Min where

import Prelude

import Data.Newtype (class Newtype)

-- | Provides a `Semigroup` based on the `min` function. If the type has a
-- | `Bounded` instance, then a `Monoid` instance is provided too. For example:
-- |
-- |     unwrap (Min 5 <> Min 6) = 5
-- |     mempty :: Min Ordering = Min GT
-- |
newtype Min a = Min a

derive instance newtypeMin :: Newtype (Min a) _

derive newtype instance eqMin :: Eq a => Eq (Min a)

instance ordMin :: Ord a => Ord (Min a) where
  compare (Min x) (Min y) = compare x y

instance semigroupMin :: Ord a => Semigroup (Min a) where
  append (Min x) (Min y) = Min (min x y)

instance monoidMin :: Bounded a => Monoid (Min a) where
  mempty = Min top

instance showMin :: Show a => Show (Min a) where
  show (Min a) = "(Min " <> show a <> ")"
