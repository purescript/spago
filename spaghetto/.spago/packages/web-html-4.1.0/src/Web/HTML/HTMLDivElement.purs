module Web.HTML.HTMLDivElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLDivElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLDivElement
fromHTMLElement = unsafeReadProtoTagged "HTMLDivElement"

fromElement :: Element -> Maybe HTMLDivElement
fromElement = unsafeReadProtoTagged "HTMLDivElement"

fromNode :: Node -> Maybe HTMLDivElement
fromNode = unsafeReadProtoTagged "HTMLDivElement"

fromChildNode :: ChildNode -> Maybe HTMLDivElement
fromChildNode = unsafeReadProtoTagged "HTMLDivElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLDivElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLDivElement"

fromParentNode :: ParentNode -> Maybe HTMLDivElement
fromParentNode = unsafeReadProtoTagged "HTMLDivElement"

fromEventTarget :: EventTarget -> Maybe HTMLDivElement
fromEventTarget = unsafeReadProtoTagged "HTMLDivElement"

toHTMLElement :: HTMLDivElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLDivElement -> Element
toElement = unsafeCoerce

toNode :: HTMLDivElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLDivElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLDivElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLDivElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLDivElement -> EventTarget
toEventTarget = unsafeCoerce
