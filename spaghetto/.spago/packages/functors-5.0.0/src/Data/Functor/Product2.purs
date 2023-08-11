module Data.Functor.Product2 where

import Prelude

import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, biapply)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Profunctor (class Profunctor, dimap)

-- | The product of two types that both take two type parameters (e.g. `Either`,
-- | `Tuple, etc.) where both type parameters are the same.
-- |
-- | ```purescript
-- | Product2 (Tuple 4 true) (Right false) :: Product2 Tuple Either Int Boolean
-- | Product2 (Tuple 4 true) (Left      8) :: Product2 Tuple Either Int Boolean
-- | ```
data Product2 :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> Type -> Type -> Type
data Product2 f g a b = Product2 (f a b) (g a b)

derive instance eqProduct2 :: (Eq (f a b), Eq (g a b)) => Eq (Product2 f g a b)

derive instance ordProduct2 :: (Ord (f a b), Ord (g a b)) => Ord (Product2 f g a b)

instance showProduct2 :: (Show (f a b), Show (g a b)) => Show (Product2 f g a b) where
  show (Product2 x y) = "(Product2 " <> show x <> " " <> show y <> ")"

instance functorProduct2 :: (Functor (f a), Functor (g a)) => Functor (Product2 f g a) where
  map f (Product2 x y) = Product2 (map f x) (map f y)

instance bifunctorProduct2 :: (Bifunctor f, Bifunctor g) => Bifunctor (Product2 f g) where
  bimap f g (Product2 x y) = Product2 (bimap f g x) (bimap f g y)

instance biapplyProduct2 :: (Biapply f, Biapply g) => Biapply (Product2 f g) where
  biapply (Product2 w x) (Product2 y z) = Product2 (biapply w y) (biapply x z)

instance biapplicativeProduct2 :: (Biapplicative f, Biapplicative g) => Biapplicative (Product2 f g) where
  bipure a b = Product2 (bipure a b) (bipure a b)

instance profunctorProduct2 :: (Profunctor f, Profunctor g) => Profunctor (Product2 f g) where
  dimap f g (Product2 x y) = Product2 (dimap f g x) (dimap f g y)
