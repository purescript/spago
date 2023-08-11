-- | This module defines the `Shop` profunctor
module Data.Lens.Internal.Shop where

import Prelude

import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))

-- | The `Shop` profunctor characterizes a `Lens`.
data Shop a b s t = Shop (s -> a) (s -> b -> t)

instance profunctorShop :: Profunctor (Shop a b) where
  dimap f g (Shop x y) = Shop (x <<< f) (\s -> g <<< y (f s))

instance strongShop :: Strong (Shop a b) where
  first (Shop x y) =
    Shop (\(Tuple a _) -> x a) (\(Tuple s c) b -> Tuple (y s b) c)
  second (Shop x y) =
    Shop (\(Tuple _ a) -> x a) (\(Tuple c s) b -> Tuple c (y s b))
