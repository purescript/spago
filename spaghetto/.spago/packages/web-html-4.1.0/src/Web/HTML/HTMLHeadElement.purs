module Web.HTML.HTMLHeadElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLHeadElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLHeadElement
fromHTMLElement = unsafeReadProtoTagged "HTMLHeadElement"

fromElement :: Element -> Maybe HTMLHeadElement
fromElement = unsafeReadProtoTagged "HTMLHeadElement"

fromNode :: Node -> Maybe HTMLHeadElement
fromNode = unsafeReadProtoTagged "HTMLHeadElement"

fromChildNode :: ChildNode -> Maybe HTMLHeadElement
fromChildNode = unsafeReadProtoTagged "HTMLHeadElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLHeadElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLHeadElement"

fromParentNode :: ParentNode -> Maybe HTMLHeadElement
fromParentNode = unsafeReadProtoTagged "HTMLHeadElement"

fromEventTarget :: EventTarget -> Maybe HTMLHeadElement
fromEventTarget = unsafeReadProtoTagged "HTMLHeadElement"

toHTMLElement :: HTMLHeadElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLHeadElement -> Element
toElement = unsafeCoerce

toNode :: HTMLHeadElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLHeadElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLHeadElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLHeadElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLHeadElement -> EventTarget
toEventTarget = unsafeCoerce
