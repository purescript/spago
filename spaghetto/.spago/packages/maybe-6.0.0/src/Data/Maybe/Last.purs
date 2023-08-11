module Data.Maybe.Last where

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

-- | Monoid returning the last (right-most) non-`Nothing` value.
-- |
-- | ``` purescript
-- | Last (Just x) <> Last (Just y) == Last (Just y)
-- | Last (Just x) <> Last Nothing == Last (Just x)
-- | Last Nothing <> Last Nothing == Last Nothing
-- | mempty :: Last _ == Last Nothing
-- | ```
newtype Last a = Last (Maybe a)

derive instance newtypeLast :: Newtype (Last a) _

derive newtype instance eqLast :: (Eq a) => Eq (Last a)

derive newtype instance eq1Last :: Eq1 Last

derive newtype instance ordLast :: (Ord a) => Ord (Last a)

derive newtype instance ord1Last :: Ord1 Last

derive newtype instance boundedLast :: (Bounded a) => Bounded (Last a)

derive newtype instance functorLast :: Functor Last

derive newtype instance invariantLast :: Invariant Last

derive newtype instance applyLast :: Apply Last

derive newtype instance applicativeLast :: Applicative Last

derive newtype instance bindLast :: Bind Last

derive newtype instance monadLast :: Monad Last

derive newtype instance extendLast :: Extend Last

instance showLast :: Show a => Show (Last a) where
  show (Last a) = "(Last " <> show a <> ")"

instance semigroupLast :: Semigroup (Last a) where
  append _ last@(Last (Just _)) = last
  append last (Last Nothing) = last

instance monoidLast :: Monoid (Last a) where
  mempty = Last Nothing

instance altLast :: Alt Last where
  alt = append

instance plusLast :: Plus Last where
  empty = mempty

instance alternativeLast :: Alternative Last
