module Web.HTML.HTMLBodyElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLBodyElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLBodyElement
fromHTMLElement = unsafeReadProtoTagged "HTMLBodyElement"

fromElement :: Element -> Maybe HTMLBodyElement
fromElement = unsafeReadProtoTagged "HTMLBodyElement"

fromNode :: Node -> Maybe HTMLBodyElement
fromNode = unsafeReadProtoTagged "HTMLBodyElement"

fromChildNode :: ChildNode -> Maybe HTMLBodyElement
fromChildNode = unsafeReadProtoTagged "HTMLBodyElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLBodyElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLBodyElement"

fromParentNode :: ParentNode -> Maybe HTMLBodyElement
fromParentNode = unsafeReadProtoTagged "HTMLBodyElement"

fromEventTarget :: EventTarget -> Maybe HTMLBodyElement
fromEventTarget = unsafeReadProtoTagged "HTMLBodyElement"

toHTMLElement :: HTMLBodyElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLBodyElement -> Element
toElement = unsafeCoerce

toNode :: HTMLBodyElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLBodyElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLBodyElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLBodyElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLBodyElement -> EventTarget
toEventTarget = unsafeCoerce
