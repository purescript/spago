module Web.DOM.NonElementParentNode
  ( NonElementParentNode
  , getElementById
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Web.DOM.Element (Element)

foreign import data NonElementParentNode :: Type

-- | The first element within node's descendants with a matching ID, or null if
-- | no such element exists.
foreign import _getElementById :: String -> NonElementParentNode -> Effect (Nullable Element)

getElementById :: String -> NonElementParentNode -> Effect (Maybe Element)
getElementById eid = map toMaybe <<< _getElementById eid
