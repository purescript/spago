module Data.Semigroup.Generic
  ( class GenericSemigroup
  , genericAppend'
  , genericAppend
  ) where

import Prelude (class Semigroup, append)
import Data.Generic.Rep

class GenericSemigroup a where
  genericAppend' :: a -> a -> a

instance genericSemigroupNoConstructors :: GenericSemigroup NoConstructors where
  genericAppend' a _ = a

instance genericSemigroupNoArguments :: GenericSemigroup NoArguments where
  genericAppend' a _ = a

instance genericSemigroupProduct :: (GenericSemigroup a, GenericSemigroup b) => GenericSemigroup (Product a b) where
  genericAppend' (Product a1 b1) (Product a2 b2) =
    Product (genericAppend' a1 a2) (genericAppend' b1 b2)

instance genericSemigroupConstructor :: GenericSemigroup a => GenericSemigroup (Constructor name a) where
  genericAppend' (Constructor a1) (Constructor a2) = Constructor (genericAppend' a1 a2)

instance genericSemigroupArgument :: Semigroup a => GenericSemigroup (Argument a) where
  genericAppend' (Argument a1) (Argument a2) = Argument (append a1 a2)

-- | A `Generic` implementation of the `append` member from the `Semigroup` type class.
genericAppend :: forall a rep. Generic a rep => GenericSemigroup rep => a -> a -> a
genericAppend x y = to (genericAppend' (from x) (from y))
