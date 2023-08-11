module Data.Lens.Prism.Maybe where

import Prelude

import Data.Either (Either(..))
import Data.Lens.Prism (Prism, prism)
import Data.Maybe (Maybe(..), maybe)

-- | Prism for the `Nothing` constructor of `Maybe`.
_Nothing :: forall a b. Prism (Maybe a) (Maybe b) Unit Unit
_Nothing = prism (const Nothing) $ maybe (Right unit) (const $ Left Nothing)

-- | Prism for the `Just` constructor of `Maybe`.
_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right
