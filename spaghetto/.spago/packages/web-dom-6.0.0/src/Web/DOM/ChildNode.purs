module Web.DOM.ChildNode where

import Prelude

import Effect (Effect)

foreign import data ChildNode :: Type

-- | Removes the node from its parent.
foreign import remove :: ChildNode -> Effect Unit
