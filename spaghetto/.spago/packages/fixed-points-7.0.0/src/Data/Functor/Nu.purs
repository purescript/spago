module Data.Functor.Nu
  ( Nu(..)
  , NuF(..)
  , unfold
  , observe
  ) where

import Prelude
import Data.Exists (Exists, mkExists, runExists)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Control.Comonad.Store (Store, store, runStore)

newtype NuF f a = NuF (Store a (f a))

-- | `Nu f` is the greatest fixed point of the functor `f`, when it exists.
newtype Nu f = Nu (Exists (NuF f))

derive instance newtypeNu :: Newtype (Nu f) _

unfold :: forall f a. a -> (a -> f a) -> Nu f
unfold pos peek = Nu $ mkExists $ NuF $ store peek pos

observe :: forall f. Functor f => Nu f -> f (Nu f)
observe (Nu e) = runExists observeF e

observeF :: forall f a. Functor f => NuF f a -> f (Nu f)
observeF (NuF x) = case runStore x of
  (Tuple peek pos) -> flip unfold peek <$> peek pos
