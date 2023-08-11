module Type.Equality
  ( class TypeEquals
  , proof
  , to
  , from
  ) where

import Prim.Coerce (class Coercible)

-- | This type class asserts that types `a` and `b`
-- | are equal.
-- |
-- | The functional dependencies and the single
-- | instance below will force the two type arguments
-- | to unify when either one is known.
-- |
-- | Note: any instance will necessarily overlap with
-- | `refl` below, so instances of this class should
-- | not be defined in libraries.
class TypeEquals :: forall k. k -> k -> Constraint
class Coercible a b <= TypeEquals a b | a -> b, b -> a where
  proof :: forall p. p a -> p b

instance refl :: TypeEquals a a where
  proof a = a

newtype To a b = To (a -> b)

to :: forall a b. TypeEquals a b => a -> b
to = case proof (To (\a -> a)) of To f -> f

newtype From a b = From (b -> a)

from :: forall a b. TypeEquals a b => b -> a
from = case proof (From (\a -> a)) of From f -> f
