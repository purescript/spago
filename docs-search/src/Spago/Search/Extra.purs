module Spago.Search.Extra where

import Prelude

import Data.Maybe (Maybe(..))

whenJust :: forall a m. Monad m => Maybe a -> (a -> m Unit) -> m Unit
whenJust (Just a) f = f a
whenJust _ _ = pure unit
