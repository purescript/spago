module Spago.Search.Extra where

import Prelude

import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe(..))
import Data.Array as Array

whenJust :: forall a m. Monad m => Maybe a -> (a -> m Unit) -> m Unit
whenJust (Just a) f = f a
whenJust _ _ = pure unit

foldMapFlipped :: forall a m f. Foldable f => Monoid m =>  f a -> (a -> m) -> m
foldMapFlipped = flip foldMap

infixr 7 foldMapFlipped as >#>
