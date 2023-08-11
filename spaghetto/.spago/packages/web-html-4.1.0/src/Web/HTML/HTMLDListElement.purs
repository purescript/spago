module Web.HTML.HTMLDListElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLDListElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLDListElement
fromHTMLElement = unsafeReadProtoTagged "HTMLDListElement"

fromElement :: Element -> Maybe HTMLDListElement
fromElement = unsafeReadProtoTagged "HTMLDListElement"

fromNode :: Node -> Maybe HTMLDListElement
fromNode = unsafeReadProtoTagged "HTMLDListElement"

fromChildNode :: ChildNode -> Maybe HTMLDListElement
fromChildNode = unsafeReadProtoTagged "HTMLDListElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLDListElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLDListElement"

fromParentNode :: ParentNode -> Maybe HTMLDListElement
fromParentNode = unsafeReadProtoTagged "HTMLDListElement"

fromEventTarget :: EventTarget -> Maybe HTMLDListElement
fromEventTarget = unsafeReadProtoTagged "HTMLDListElement"

toHTMLElement :: HTMLDListElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLDListElement -> Element
toElement = unsafeCoerce

toNode :: HTMLDListElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLDListElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLDListElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLDListElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLDListElement -> EventTarget
toEventTarget = unsafeCoerce
