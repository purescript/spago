module Data.Divide where

import Prelude

import Data.Comparison (Comparison(..))
import Data.Equivalence (Equivalence(..))
import Data.Functor.Contravariant (class Contravariant)
import Data.Op (Op(..))
import Data.Predicate (Predicate(..))
import Data.Tuple (Tuple(..))

-- | `Divide` is the contravariant analogue of `Apply`.
-- |
-- | For example, to test equality of `Point`s, we can use the `Divide` instance
-- | for `Equivalence`:
-- |
-- | ```purescript
-- | type Point = Tuple Int Int
-- |
-- | pointEquiv :: Equivalence Point
-- | pointEquiv = divided defaultEquivalence defaultEquivalence
-- | ```
class Contravariant f <= Divide f where
  divide :: forall a b c. (a -> Tuple b c) -> f b -> f c -> f a

instance divideComparison :: Divide Comparison where
  divide f (Comparison g) (Comparison h) = Comparison \a b -> case f a of
    Tuple a' a'' -> case f b of
      Tuple b' b'' -> g a' b' <> h a'' b''

instance divideEquivalence :: Divide Equivalence where
  divide f (Equivalence g) (Equivalence h) = Equivalence \a b -> case f a of
    Tuple a' a'' -> case f b of
      Tuple b' b'' -> g a' b' && h a'' b''

instance dividePredicate :: Divide Predicate where
  divide f (Predicate g) (Predicate h) = Predicate \a -> case f a of
    Tuple b c -> g b && h c

instance divideOp :: Semigroup r => Divide (Op r) where
  divide f (Op g) (Op h) = Op \a -> case f a of
    Tuple b c -> g b <> h c

-- | `divided = divide id`
divided :: forall f a b. Divide f => f a -> f b -> f (Tuple a b)
divided = divide identity
