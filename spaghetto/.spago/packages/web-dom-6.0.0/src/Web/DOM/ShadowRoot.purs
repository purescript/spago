module Web.DOM.ShadowRoot
  ( ShadowRoot
  , ShadowRootMode (..)
  , toNode
  , host
  , mode
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Internal.Types (Element, Node)

foreign import data ShadowRoot :: Type

toNode :: ShadowRoot -> Node
toNode = unsafeCoerce

data ShadowRootMode = Open | Closed

instance showShadowRootMode :: Show ShadowRootMode where
  show Open = "open"
  show Closed = "closed"

mode :: ShadowRoot -> Maybe ShadowRootMode
mode = modeFromString <<< _mode
  where
    modeFromString = case _ of
      "open" -> Just Open
      "closed" -> Just Closed
      _ -> Nothing

foreign import host :: ShadowRoot -> Effect Element
foreign import _mode :: ShadowRoot -> String
