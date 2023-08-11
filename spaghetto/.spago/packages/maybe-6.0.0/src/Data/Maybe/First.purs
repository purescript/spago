module Data.Maybe.First where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.Plus (class Plus)

import Data.Eq (class Eq1)
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)

-- | Monoid returning the first (left-most) non-`Nothing` value.
-- |
-- | ``` purescript
-- | First (Just x) <> First (Just y) == First (Just x)
-- | First Nothing <> First (Just y) == First (Just y)
-- | First Nothing <> First Nothing == First Nothing
-- | mempty :: First _ == First Nothing
-- | ```
newtype First a = First (Maybe a)

derive instance newtypeFirst :: Newtype (First a) _

derive newtype instance eqFirst :: (Eq a) => Eq (First a)

derive newtype instance eq1First :: Eq1 First

derive newtype instance ordFirst :: (Ord a) => Ord (First a)

derive newtype instance ord1First :: Ord1 First

derive newtype instance boundedFirst :: (Bounded a) => Bounded (First a)

derive newtype instance functorFirst :: Functor First

derive newtype instance invariantFirst :: Invariant First

derive newtype instance applyFirst :: Apply First

derive newtype instance applicativeFirst :: Applicative First

derive newtype instance bindFirst :: Bind First

derive newtype instance monadFirst :: Monad First

derive newtype instance extendFirst :: Extend First

instance showFirst :: (Show a) => Show (First a) where
  show (First a) = "First (" <> show a <> ")"

instance semigroupFirst :: Semigroup (First a) where
  append first@(First (Just _)) _ = first
  append _ second = second

instance monoidFirst :: Monoid (First a) where
  mempty = First Nothing

instance altFirst :: Alt First where
  alt = append

instance plusFirst :: Plus First where
  empty = mempty

instance alternativeFirst :: Alternative First
