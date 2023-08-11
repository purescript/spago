module Data.Semiring.Generic where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)

class GenericSemiring a where
  genericAdd' :: a -> a -> a
  genericZero' :: a
  genericMul' :: a -> a -> a
  genericOne' :: a

instance genericSemiringNoArguments :: GenericSemiring NoArguments where
  genericAdd' _ _ = NoArguments
  genericZero' = NoArguments
  genericMul' _ _ = NoArguments
  genericOne' = NoArguments

instance genericSemiringArgument :: Semiring a => GenericSemiring (Argument a) where
  genericAdd' (Argument x) (Argument y) = Argument (add x y)
  genericZero' = Argument zero
  genericMul' (Argument x) (Argument y) = Argument (mul x y)
  genericOne' = Argument one

instance genericSemiringProduct :: (GenericSemiring a, GenericSemiring b) => GenericSemiring (Product a b) where
  genericAdd' (Product a1 b1) (Product a2 b2) = Product (genericAdd' a1 a2) (genericAdd' b1 b2)
  genericZero' = Product genericZero' genericZero'
  genericMul' (Product a1 b1) (Product a2 b2) = Product (genericMul' a1 a2) (genericMul' b1 b2)
  genericOne' = Product genericOne' genericOne'

instance genericSemiringConstructor :: GenericSemiring a => GenericSemiring (Constructor name a) where
  genericAdd' (Constructor a1) (Constructor a2) = Constructor (genericAdd' a1 a2)
  genericZero' = Constructor genericZero'
  genericMul' (Constructor a1) (Constructor a2) = Constructor (genericMul' a1 a2)
  genericOne' = Constructor genericOne'

-- | A `Generic` implementation of the `zero` member from the `Semiring` type class.
genericZero :: forall a rep. Generic a rep => GenericSemiring rep => a
genericZero = to genericZero'

-- | A `Generic` implementation of the `one` member from the `Semiring` type class.
genericOne :: forall a rep. Generic a rep => GenericSemiring rep => a
genericOne = to genericOne'

-- | A `Generic` implementation of the `add` member from the `Semiring` type class.
genericAdd :: forall a rep. Generic a rep => GenericSemiring rep => a -> a -> a
genericAdd x y = to $ from x `genericAdd'` from y

-- | A `Generic` implementation of the `mul` member from the `Semiring` type class.
genericMul :: forall a rep. Generic a rep => GenericSemiring rep => a -> a -> a
genericMul x y = to $ from x `genericMul'` from y