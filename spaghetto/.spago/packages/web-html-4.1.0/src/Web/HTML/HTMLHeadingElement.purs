module Web.HTML.HTMLHeadingElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLHeadingElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLHeadingElement
fromHTMLElement = unsafeReadProtoTagged "HTMLHeadingElement"

fromElement :: Element -> Maybe HTMLHeadingElement
fromElement = unsafeReadProtoTagged "HTMLHeadingElement"

fromNode :: Node -> Maybe HTMLHeadingElement
fromNode = unsafeReadProtoTagged "HTMLHeadingElement"

fromChildNode :: ChildNode -> Maybe HTMLHeadingElement
fromChildNode = unsafeReadProtoTagged "HTMLHeadingElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLHeadingElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLHeadingElement"

fromParentNode :: ParentNode -> Maybe HTMLHeadingElement
fromParentNode = unsafeReadProtoTagged "HTMLHeadingElement"

fromEventTarget :: EventTarget -> Maybe HTMLHeadingElement
fromEventTarget = unsafeReadProtoTagged "HTMLHeadingElement"

toHTMLElement :: HTMLHeadingElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLHeadingElement -> Element
toElement = unsafeCoerce

toNode :: HTMLHeadingElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLHeadingElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLHeadingElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLHeadingElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLHeadingElement -> EventTarget
toEventTarget = unsafeCoerce
