module Web.HTML.HTMLTableCaptionElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTableCaptionElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLTableCaptionElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTableCaptionElement"

fromElement :: Element -> Maybe HTMLTableCaptionElement
fromElement = unsafeReadProtoTagged "HTMLTableCaptionElement"

fromNode :: Node -> Maybe HTMLTableCaptionElement
fromNode = unsafeReadProtoTagged "HTMLTableCaptionElement"

fromChildNode :: ChildNode -> Maybe HTMLTableCaptionElement
fromChildNode = unsafeReadProtoTagged "HTMLTableCaptionElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTableCaptionElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTableCaptionElement"

fromParentNode :: ParentNode -> Maybe HTMLTableCaptionElement
fromParentNode = unsafeReadProtoTagged "HTMLTableCaptionElement"

fromEventTarget :: EventTarget -> Maybe HTMLTableCaptionElement
fromEventTarget = unsafeReadProtoTagged "HTMLTableCaptionElement"

toHTMLElement :: HTMLTableCaptionElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTableCaptionElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTableCaptionElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTableCaptionElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTableCaptionElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTableCaptionElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTableCaptionElement -> EventTarget
toEventTarget = unsafeCoerce
