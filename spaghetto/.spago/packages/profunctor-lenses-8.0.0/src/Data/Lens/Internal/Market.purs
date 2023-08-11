-- | This module defines the `Market` profunctor
module Data.Lens.Internal.Market where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)

-- | The `Market` profunctor characterizes a `Prism`.
data Market a b s t = Market (b -> t) (s -> Either t a)

instance functorMarket :: Functor (Market a b s) where
  map f (Market a b) = Market (f <<< a) (lmap f <<< b)

instance profunctorMarket :: Profunctor (Market a b) where
  dimap f g (Market a b) = Market (g <<< a) (lmap g <<< b <<< f)

instance choiceMarket :: Choice (Market a b) where
  left (Market x y) =
    Market (Left <<< x) (either (lmap Left <<< y) (Left <<< Right))
  right (Market x y) =
    Market (Right <<< x) (either (Left <<< Left) (lmap Right <<< y))
