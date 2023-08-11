module Web.HTML.HTMLVideoElement where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLMediaElement (HTMLMediaElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLVideoElement :: Type

fromHTMLMediaElement :: HTMLMediaElement -> Maybe HTMLVideoElement
fromHTMLMediaElement = unsafeReadProtoTagged "HTMLVideoElement"

fromHTMLElement :: HTMLElement -> Maybe HTMLVideoElement
fromHTMLElement = unsafeReadProtoTagged "HTMLVideoElement"

fromElement :: Element -> Maybe HTMLVideoElement
fromElement = unsafeReadProtoTagged "HTMLVideoElement"

fromNode :: Node -> Maybe HTMLVideoElement
fromNode = unsafeReadProtoTagged "HTMLVideoElement"

fromChildNode :: ChildNode -> Maybe HTMLVideoElement
fromChildNode = unsafeReadProtoTagged "HTMLVideoElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLVideoElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLVideoElement"

fromParentNode :: ParentNode -> Maybe HTMLVideoElement
fromParentNode = unsafeReadProtoTagged "HTMLVideoElement"

fromEventTarget :: EventTarget -> Maybe HTMLVideoElement
fromEventTarget = unsafeReadProtoTagged "HTMLVideoElement"

toHTMLMediaElement :: HTMLVideoElement -> HTMLMediaElement
toHTMLMediaElement = unsafeCoerce

toHTMLElement :: HTMLVideoElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLVideoElement -> Element
toElement = unsafeCoerce

toNode :: HTMLVideoElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLVideoElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLVideoElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLVideoElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLVideoElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import width :: HTMLVideoElement -> Effect Int
foreign import setWidth :: Int -> HTMLVideoElement -> Effect Unit

foreign import height :: HTMLVideoElement -> Effect Int
foreign import setHeight :: Int -> HTMLVideoElement -> Effect Unit

foreign import videoWidth :: HTMLVideoElement -> Effect Int
foreign import videoHeight :: HTMLVideoElement -> Effect Int

foreign import poster :: HTMLVideoElement -> Effect String
foreign import setPoster :: String -> HTMLVideoElement -> Effect Unit
