module Data.Profunctor.Costrong where

import Data.Tuple (Tuple)
import Data.Profunctor (class Profunctor)

-- | The `Costrong` class provides the dual operations of the `Strong` class.
class Profunctor p <= Costrong p where
  unfirst :: forall a b c. p (Tuple a c) (Tuple b c) -> p a b
  unsecond :: forall a b c. p (Tuple a b) (Tuple a c) -> p b c
