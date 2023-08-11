module Data.Ring.Generic where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)

class GenericRing a where
  genericSub' :: a -> a -> a

instance genericRingNoArguments :: GenericRing NoArguments where
  genericSub' _ _ = NoArguments

instance genericRingArgument :: Ring a => GenericRing (Argument a) where
  genericSub' (Argument x) (Argument y) = Argument (sub x y)

instance genericRingProduct :: (GenericRing a, GenericRing b) => GenericRing (Product a b) where
  genericSub' (Product a1 b1) (Product a2 b2) = Product (genericSub' a1 a2) (genericSub' b1 b2)

instance genericRingConstructor :: GenericRing a => GenericRing (Constructor name a) where
  genericSub' (Constructor a1) (Constructor a2) = Constructor (genericSub' a1 a2)

-- | A `Generic` implementation of the `sub` member from the `Ring` type class.
genericSub :: forall a rep. Generic a rep => GenericRing rep => a -> a -> a
genericSub x y = to $ from x `genericSub'` from y