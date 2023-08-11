module Data.Ord.Down where

import Prelude

import Data.Newtype (class Newtype)
import Data.Ordering (invert)

-- | A newtype wrapper which provides a reversed `Ord` instance. For example:
-- |
-- |     sortBy (comparing Down) [1,2,3] = [3,2,1]
-- |
newtype Down a = Down a

derive instance newtypeDown :: Newtype (Down a) _

derive newtype instance eqDown :: Eq a => Eq (Down a)

instance ordDown :: Ord a => Ord (Down a) where
  compare (Down x) (Down y) = invert (compare x y)

instance boundedDown :: Bounded a => Bounded (Down a) where
  top = Down bottom
  bottom = Down top

instance showDown :: Show a => Show (Down a) where
  show (Down a) = "(Down " <> show a <> ")"
