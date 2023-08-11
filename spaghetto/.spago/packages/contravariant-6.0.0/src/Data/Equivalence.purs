module Data.Equivalence where

import Prelude

import Data.Comparison (Comparison(..))
import Data.Function (on)
import Data.Functor.Contravariant (class Contravariant)
import Data.Newtype (class Newtype)

-- | An adaptor allowing `>$<` to map over the inputs of an equivalence
-- | relation.
newtype Equivalence a = Equivalence (a -> a -> Boolean)

derive instance newtypeEquivalence :: Newtype (Equivalence a) _

instance contravariantEquivalence :: Contravariant Equivalence where
  cmap f (Equivalence g) = Equivalence (g `on` f)

instance semigroupEquivalence :: Semigroup (Equivalence a) where
  append (Equivalence p) (Equivalence q) = Equivalence (\a b -> p a b && q a b)

instance monoidEquivalence :: Monoid (Equivalence a) where
  mempty = Equivalence (\_ _ -> true)

-- | The default equivalence relation for any values with an `Eq` instance.
defaultEquivalence :: forall a. Eq a => Equivalence a
defaultEquivalence = Equivalence eq

-- | An equivalence relation for any `Comparison`.
comparisonEquivalence :: forall a. Comparison a -> Equivalence a
comparisonEquivalence (Comparison p) = Equivalence (\a b -> p a b == EQ)
