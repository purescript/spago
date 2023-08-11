module Data.Decidable where

import Prelude

import Data.Comparison (Comparison(..))
import Data.Decide (class Decide)
import Data.Divisible (class Divisible)
import Data.Equivalence (Equivalence(..))
import Data.Op (Op(..))
import Data.Predicate (Predicate(..))

-- | `Decidable` is the contravariant analogue of `Alternative`.
class (Decide f, Divisible f) <= Decidable f where
  lose :: forall a. (a -> Void) -> f a

instance decidableComparison :: Decidable Comparison where
  lose f = Comparison \a _ -> absurd (f a)

instance decidableEquivalence :: Decidable Equivalence where
  lose f = Equivalence \a -> absurd (f a)

instance decidablePredicate :: Decidable Predicate where
  lose f = Predicate \a -> absurd (f a)

instance decidableOp :: Monoid r => Decidable (Op r) where
  lose f = Op \a -> absurd (f a)

lost :: forall f. Decidable f => f Void
lost = lose identity
