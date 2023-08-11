module Data.HeytingAlgebra.Generic where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)
import Data.HeytingAlgebra (ff, implies, tt)

class GenericHeytingAlgebra a where
  genericFF' :: a
  genericTT' :: a
  genericImplies' :: a -> a -> a
  genericConj' :: a -> a -> a
  genericDisj' :: a -> a -> a
  genericNot' :: a -> a

instance genericHeytingAlgebraNoArguments :: GenericHeytingAlgebra NoArguments where
  genericFF' = NoArguments
  genericTT' = NoArguments
  genericImplies' _ _ = NoArguments
  genericConj' _ _ = NoArguments
  genericDisj' _ _ = NoArguments
  genericNot' _ = NoArguments

instance genericHeytingAlgebraArgument :: HeytingAlgebra a => GenericHeytingAlgebra (Argument a) where
  genericFF' = Argument ff
  genericTT' = Argument tt
  genericImplies' (Argument x) (Argument y) = Argument (implies x y)
  genericConj' (Argument x) (Argument y) = Argument (conj x y)
  genericDisj' (Argument x) (Argument y) = Argument (disj x y)
  genericNot' (Argument x) = Argument (not x)

instance genericHeytingAlgebraProduct :: (GenericHeytingAlgebra a, GenericHeytingAlgebra b) => GenericHeytingAlgebra (Product a b) where
  genericFF' = Product genericFF' genericFF'
  genericTT' = Product genericTT' genericTT'
  genericImplies' (Product a1 b1) (Product a2 b2) = Product (genericImplies' a1 a2) (genericImplies' b1 b2)
  genericConj' (Product a1 b1) (Product a2 b2) = Product (genericConj' a1 a2) (genericConj' b1 b2)
  genericDisj' (Product a1 b1) (Product a2 b2) = Product (genericDisj' a1 a2) (genericDisj' b1 b2)
  genericNot' (Product a b) = Product (genericNot' a) (genericNot' b)

instance genericHeytingAlgebraConstructor :: GenericHeytingAlgebra a => GenericHeytingAlgebra (Constructor name a) where
  genericFF' = Constructor genericFF'
  genericTT' = Constructor genericTT'
  genericImplies' (Constructor a1) (Constructor a2) = Constructor (genericImplies' a1 a2)
  genericConj' (Constructor a1) (Constructor a2) = Constructor (genericConj' a1 a2)
  genericDisj' (Constructor a1) (Constructor a2) = Constructor (genericDisj' a1 a2)
  genericNot' (Constructor a) = Constructor (genericNot' a)

-- | A `Generic` implementation of the `ff` member from the `HeytingAlgebra` type class.
genericFF :: forall a rep. Generic a rep => GenericHeytingAlgebra rep => a
genericFF = to genericFF'

-- | A `Generic` implementation of the `tt` member from the `HeytingAlgebra` type class.
genericTT :: forall a rep. Generic a rep => GenericHeytingAlgebra rep => a
genericTT = to genericTT'

-- | A `Generic` implementation of the `implies` member from the `HeytingAlgebra` type class.
genericImplies :: forall a rep. Generic a rep => GenericHeytingAlgebra rep => a -> a -> a
genericImplies x y = to $ from x `genericImplies'` from y

-- | A `Generic` implementation of the `conj` member from the `HeytingAlgebra` type class.
genericConj :: forall a rep. Generic a rep => GenericHeytingAlgebra rep => a -> a -> a
genericConj x y = to $ from x `genericConj'` from y

-- | A `Generic` implementation of the `disj` member from the `HeytingAlgebra` type class.
genericDisj :: forall a rep. Generic a rep => GenericHeytingAlgebra rep => a -> a -> a
genericDisj x y = to $ from x `genericDisj'` from y

-- | A `Generic` implementation of the `not` member from the `HeytingAlgebra` type class.
genericNot :: forall a rep. Generic a rep => GenericHeytingAlgebra rep => a -> a
genericNot x = to $ genericNot' (from x)