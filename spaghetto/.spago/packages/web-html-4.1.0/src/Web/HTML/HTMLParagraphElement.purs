module Web.HTML.HTMLParagraphElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLParagraphElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLParagraphElement
fromHTMLElement = unsafeReadProtoTagged "HTMLParagraphElement"

fromElement :: Element -> Maybe HTMLParagraphElement
fromElement = unsafeReadProtoTagged "HTMLParagraphElement"

fromNode :: Node -> Maybe HTMLParagraphElement
fromNode = unsafeReadProtoTagged "HTMLParagraphElement"

fromChildNode :: ChildNode -> Maybe HTMLParagraphElement
fromChildNode = unsafeReadProtoTagged "HTMLParagraphElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLParagraphElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLParagraphElement"

fromParentNode :: ParentNode -> Maybe HTMLParagraphElement
fromParentNode = unsafeReadProtoTagged "HTMLParagraphElement"

fromEventTarget :: EventTarget -> Maybe HTMLParagraphElement
fromEventTarget = unsafeReadProtoTagged "HTMLParagraphElement"

toHTMLElement :: HTMLParagraphElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLParagraphElement -> Element
toElement = unsafeCoerce

toNode :: HTMLParagraphElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLParagraphElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLParagraphElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLParagraphElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLParagraphElement -> EventTarget
toEventTarget = unsafeCoerce
