module Data.Enum.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, elements)
import Data.Enum (class BoundedEnum, succ, enumFromTo)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))

-- | Create a random generator for a finite enumeration.
genBoundedEnum :: forall m a. MonadGen m => BoundedEnum a => m a
genBoundedEnum =
  case succ bottom of
    Just a →
      let possibilities = enumFromTo a top :: Array a
      in elements (bottom :| possibilities)
    Nothing →
      pure bottom
