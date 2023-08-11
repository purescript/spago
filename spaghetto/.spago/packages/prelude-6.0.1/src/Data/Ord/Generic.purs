module Data.Ord.Generic
  ( class GenericOrd
  , genericCompare'
  , genericCompare
  ) where

import Prelude (class Ord, compare, Ordering(..))
import Data.Generic.Rep

class GenericOrd a where
  genericCompare' :: a -> a -> Ordering

instance genericOrdNoConstructors :: GenericOrd NoConstructors where
  genericCompare' _ _ = EQ

instance genericOrdNoArguments :: GenericOrd NoArguments where
  genericCompare' _ _ = EQ

instance genericOrdSum :: (GenericOrd a, GenericOrd b) => GenericOrd (Sum a b) where
  genericCompare' (Inl a1) (Inl a2) = genericCompare' a1 a2
  genericCompare' (Inr b1) (Inr b2) = genericCompare' b1 b2
  genericCompare' (Inl _) (Inr _) = LT
  genericCompare' (Inr _) (Inl _) = GT

instance genericOrdProduct :: (GenericOrd a, GenericOrd b) => GenericOrd (Product a b) where
  genericCompare' (Product a1 b1) (Product a2 b2) =
    case genericCompare' a1 a2 of
      EQ -> genericCompare' b1 b2
      other -> other

instance genericOrdConstructor :: GenericOrd a => GenericOrd (Constructor name a) where
  genericCompare' (Constructor a1) (Constructor a2) = genericCompare' a1 a2

instance genericOrdArgument :: Ord a => GenericOrd (Argument a) where
  genericCompare' (Argument a1) (Argument a2) = compare a1 a2

-- | A `Generic` implementation of the `compare` member from the `Ord` type class.
genericCompare :: forall a rep. Generic a rep => GenericOrd rep => a -> a -> Ordering
genericCompare x y = genericCompare' (from x) (from y)
