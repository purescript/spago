module Web.HTML.HTMLSpanElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLSpanElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLSpanElement
fromHTMLElement = unsafeReadProtoTagged "HTMLSpanElement"

fromElement :: Element -> Maybe HTMLSpanElement
fromElement = unsafeReadProtoTagged "HTMLSpanElement"

fromNode :: Node -> Maybe HTMLSpanElement
fromNode = unsafeReadProtoTagged "HTMLSpanElement"

fromChildNode :: ChildNode -> Maybe HTMLSpanElement
fromChildNode = unsafeReadProtoTagged "HTMLSpanElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLSpanElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLSpanElement"

fromParentNode :: ParentNode -> Maybe HTMLSpanElement
fromParentNode = unsafeReadProtoTagged "HTMLSpanElement"

fromEventTarget :: EventTarget -> Maybe HTMLSpanElement
fromEventTarget = unsafeReadProtoTagged "HTMLSpanElement"

toHTMLElement :: HTMLSpanElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLSpanElement -> Element
toElement = unsafeCoerce

toNode :: HTMLSpanElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLSpanElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLSpanElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLSpanElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLSpanElement -> EventTarget
toEventTarget = unsafeCoerce
