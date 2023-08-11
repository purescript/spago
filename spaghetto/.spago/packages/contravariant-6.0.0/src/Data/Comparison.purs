module Data.Comparison where

import Prelude

import Data.Function (on)
import Data.Functor.Contravariant (class Contravariant)
import Data.Newtype (class Newtype)

-- | An adaptor allowing `>$<` to map over the inputs of a comparison function.
newtype Comparison a = Comparison (a -> a -> Ordering)

derive instance newtypeComparison :: Newtype (Comparison a) _

instance contravariantComparison :: Contravariant Comparison where
  cmap f (Comparison g) = Comparison (g `on` f)

instance semigroupComparison :: Semigroup (Comparison a) where
  append (Comparison p) (Comparison q) = Comparison (p <> q)

instance monoidComparison :: Monoid (Comparison a) where
  mempty = Comparison (\_ _ -> EQ)

-- | The default comparison for any values with an `Ord` instance.
defaultComparison :: forall a. Ord a => Comparison a
defaultComparison = Comparison compare
