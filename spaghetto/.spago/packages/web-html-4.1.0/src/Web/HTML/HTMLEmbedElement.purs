module Web.HTML.HTMLEmbedElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLEmbedElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLEmbedElement
fromHTMLElement = unsafeReadProtoTagged "HTMLEmbedElement"

fromElement :: Element -> Maybe HTMLEmbedElement
fromElement = unsafeReadProtoTagged "HTMLEmbedElement"

fromNode :: Node -> Maybe HTMLEmbedElement
fromNode = unsafeReadProtoTagged "HTMLEmbedElement"

fromChildNode :: ChildNode -> Maybe HTMLEmbedElement
fromChildNode = unsafeReadProtoTagged "HTMLEmbedElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLEmbedElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLEmbedElement"

fromParentNode :: ParentNode -> Maybe HTMLEmbedElement
fromParentNode = unsafeReadProtoTagged "HTMLEmbedElement"

fromEventTarget :: EventTarget -> Maybe HTMLEmbedElement
fromEventTarget = unsafeReadProtoTagged "HTMLEmbedElement"

toHTMLElement :: HTMLEmbedElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLEmbedElement -> Element
toElement = unsafeCoerce

toNode :: HTMLEmbedElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLEmbedElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLEmbedElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLEmbedElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLEmbedElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import src :: HTMLEmbedElement -> Effect String
foreign import setSrc :: String -> HTMLEmbedElement -> Effect Unit

foreign import type_ :: HTMLEmbedElement -> Effect String
foreign import setType :: String -> HTMLEmbedElement -> Effect Unit

foreign import width :: HTMLEmbedElement -> Effect String
foreign import setWidth :: String -> HTMLEmbedElement -> Effect Unit

foreign import height :: HTMLEmbedElement -> Effect String
foreign import setHeight :: String -> HTMLEmbedElement -> Effect Unit
