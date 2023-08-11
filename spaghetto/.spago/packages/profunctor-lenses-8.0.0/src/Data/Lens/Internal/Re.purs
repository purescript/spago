-- | This module defines the `Re` profunctor
module Data.Lens.Internal.Re where

import Prelude

import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, left, right)
import Data.Profunctor.Cochoice (class Cochoice, unleft, unright)
import Data.Profunctor.Costrong (class Costrong, unfirst, unsecond)
import Data.Profunctor.Strong (class Strong, first, second)

newtype Re :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Type
newtype Re p s t a b = Re (p b a -> p t s)

derive instance newtypeRe :: Newtype (Re p s t a b) _

instance profunctorRe :: Profunctor p => Profunctor (Re p s t) where
  dimap f g (Re r) = Re (r <<< dimap g f)

instance choiceRe :: Choice p => Cochoice (Re p s t) where
  unleft (Re r) = Re (r <<< left)
  unright (Re r) = Re (r <<< right)

instance cochoiceRe :: Cochoice p => Choice (Re p s t) where
  left (Re r) = Re (r <<< unleft)
  right (Re r) = Re (r <<< unright)

instance strongRe :: Strong p => Costrong (Re p s t) where
  unfirst (Re r) = Re (r <<< first)
  unsecond (Re r) = Re (r <<< second)

instance costrongRe :: Costrong p => Strong (Re p s t) where
  first (Re r) = Re (r <<< unfirst)
  second (Re r) = Re (r <<< unsecond)
