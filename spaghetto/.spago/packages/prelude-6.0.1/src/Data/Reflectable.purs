module Data.Reflectable
  ( class Reflectable
  , class Reifiable
  , reflectType
  , reifyType
  ) where

import Data.Ord (Ordering)
import Type.Proxy (Proxy(..))

-- | A type-class for reflectable types.
-- |
-- | Instances for the following kinds are solved by the compiler:
-- | * Boolean
-- | * Int
-- | * Ordering
-- | * Symbol
class Reflectable :: forall k. k -> Type -> Constraint
class Reflectable v t | v -> t where
  -- | Reflect a type `v` to its term-level representation.
  reflectType :: Proxy v -> t

-- | A type class for reifiable types.
-- |
-- | Instances of this type class correspond to the `t` synthesized
-- | by the compiler when solving the `Reflectable` type class.
class Reifiable :: Type -> Constraint
class Reifiable t

instance Reifiable Boolean
instance Reifiable Int
instance Reifiable Ordering
instance Reifiable String

-- local definition for use in `reifyType`
foreign import unsafeCoerce :: forall a b. a -> b

-- | Reify a value of type `t` such that it can be consumed by a
-- | function constrained by the `Reflectable` type class. For
-- | example:
-- |
-- | ```purs
-- | twiceFromType :: forall v. Reflectable v Int => Proxy v -> Int
-- | twiceFromType = (_ * 2) <<< reflectType
-- |
-- | twiceOfTerm :: Int
-- | twiceOfTerm = reifyType 21 twiceFromType
-- | ```
reifyType :: forall t r. Reifiable t => t -> (forall v. Reflectable v t => Proxy v -> r) -> r
reifyType s f = coerce f { reflectType: \_ -> s } Proxy
  where
  coerce
    :: (forall v. Reflectable v t => Proxy v -> r)
    -> { reflectType :: Proxy _ -> t }
    -> Proxy _
    -> r
  coerce = unsafeCoerce
