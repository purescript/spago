module Data.Eq.Generic
  ( class GenericEq
  , genericEq'
  , genericEq
  ) where

import Prelude (class Eq, (==), (&&))
import Data.Generic.Rep

class GenericEq a where
  genericEq' :: a -> a -> Boolean

instance genericEqNoConstructors :: GenericEq NoConstructors where
  genericEq' _ _ = true

instance genericEqNoArguments :: GenericEq NoArguments where
  genericEq' _ _ = true

instance genericEqSum :: (GenericEq a, GenericEq b) => GenericEq (Sum a b) where
  genericEq' (Inl a1) (Inl a2) = genericEq' a1 a2
  genericEq' (Inr b1) (Inr b2) = genericEq' b1 b2
  genericEq' _ _ = false

instance genericEqProduct :: (GenericEq a, GenericEq b) => GenericEq (Product a b) where
  genericEq' (Product a1 b1) (Product a2 b2) = genericEq' a1 a2 && genericEq' b1 b2

instance genericEqConstructor :: GenericEq a => GenericEq (Constructor name a) where
  genericEq' (Constructor a1) (Constructor a2) = genericEq' a1 a2

instance genericEqArgument :: Eq a => GenericEq (Argument a) where
  genericEq' (Argument a1) (Argument a2) = a1 == a2

-- | A `Generic` implementation of the `eq` member from the `Eq` type class.
genericEq :: forall a rep. Generic a rep => GenericEq rep => a -> a -> Boolean
genericEq x y = genericEq' (from x) (from y)
