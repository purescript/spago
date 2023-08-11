module Web.HTML.HTMLTrackElement
  ( HTMLTrackElement
  , fromHTMLElement
  , fromElement
  , fromNode
  , fromChildNode
  , fromNonDocumentTypeChildNode
  , fromParentNode
  , fromEventTarget
  , toHTMLElement
  , toElement
  , toNode
  , toChildNode
  , toNonDocumentTypeChildNode
  , toParentNode
  , toEventTarget
  , kind
  , setKind
  , src
  , setSrc
  , srclang
  , setSrclang
  , label
  , setLabel
  , default
  , setDefault
  , readyState
  ) where

import Prelude

import Data.Enum (toEnum)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLTrackElement.ReadyState (ReadyState)
import Web.HTML.HTMLTrackElement.ReadyState as ReadyState
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTrackElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLTrackElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTrackElement"

fromElement :: Element -> Maybe HTMLTrackElement
fromElement = unsafeReadProtoTagged "HTMLTrackElement"

fromNode :: Node -> Maybe HTMLTrackElement
fromNode = unsafeReadProtoTagged "HTMLTrackElement"

fromChildNode :: ChildNode -> Maybe HTMLTrackElement
fromChildNode = unsafeReadProtoTagged "HTMLTrackElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTrackElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTrackElement"

fromParentNode :: ParentNode -> Maybe HTMLTrackElement
fromParentNode = unsafeReadProtoTagged "HTMLTrackElement"

fromEventTarget :: EventTarget -> Maybe HTMLTrackElement
fromEventTarget = unsafeReadProtoTagged "HTMLTrackElement"

toHTMLElement :: HTMLTrackElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTrackElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTrackElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTrackElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTrackElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTrackElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTrackElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import kind :: HTMLTrackElement -> Effect String
foreign import setKind :: String -> HTMLTrackElement -> Effect Unit

foreign import src :: HTMLTrackElement -> Effect String
foreign import setSrc :: String -> HTMLTrackElement -> Effect Unit

foreign import srclang :: HTMLTrackElement -> Effect String
foreign import setSrclang :: String -> HTMLTrackElement -> Effect Unit

foreign import label :: HTMLTrackElement -> Effect String
foreign import setLabel :: String -> HTMLTrackElement -> Effect Unit

foreign import default :: HTMLTrackElement -> Effect Boolean
foreign import setDefault :: Boolean -> HTMLTrackElement -> Effect Unit

readyState :: HTMLTrackElement -> Effect ReadyState
readyState el = map (fromMaybe ReadyState.None <<< toEnum) $ runEffectFn1 _readyState el

foreign import _readyState :: EffectFn1 HTMLTrackElement Int

--   readonly attribute TextTrack track;
