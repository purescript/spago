module Web.DOM.NonDocumentTypeChildNode
  ( NonDocumentTypeChildNode
  , previousElementSibling
  , nextElementSibling
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Web.DOM.Internal.Types (Element)

foreign import data NonDocumentTypeChildNode :: Type

-- | The previous sibling that is an element, or Nothing if no such element exists.
previousElementSibling :: NonDocumentTypeChildNode -> Effect (Maybe Element)
previousElementSibling = map toMaybe <<< _previousElementSibling

foreign import _previousElementSibling :: NonDocumentTypeChildNode -> Effect (Nullable Element)

-- | The next sibling that is an element, or Nothing if no such element exists.
nextElementSibling :: NonDocumentTypeChildNode -> Effect (Maybe Element)
nextElementSibling = map toMaybe <<< _nextElementSibling

foreign import _nextElementSibling :: NonDocumentTypeChildNode -> Effect (Nullable Element)
