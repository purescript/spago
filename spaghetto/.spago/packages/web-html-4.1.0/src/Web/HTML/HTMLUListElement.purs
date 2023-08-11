module Web.HTML.HTMLUListElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLUListElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLUListElement
fromHTMLElement = unsafeReadProtoTagged "HTMLUListElement"

fromElement :: Element -> Maybe HTMLUListElement
fromElement = unsafeReadProtoTagged "HTMLUListElement"

fromNode :: Node -> Maybe HTMLUListElement
fromNode = unsafeReadProtoTagged "HTMLUListElement"

fromChildNode :: ChildNode -> Maybe HTMLUListElement
fromChildNode = unsafeReadProtoTagged "HTMLUListElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLUListElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLUListElement"

fromParentNode :: ParentNode -> Maybe HTMLUListElement
fromParentNode = unsafeReadProtoTagged "HTMLUListElement"

fromEventTarget :: EventTarget -> Maybe HTMLUListElement
fromEventTarget = unsafeReadProtoTagged "HTMLUListElement"

toHTMLElement :: HTMLUListElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLUListElement -> Element
toElement = unsafeCoerce

toNode :: HTMLUListElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLUListElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLUListElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLUListElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLUListElement -> EventTarget
toEventTarget = unsafeCoerce
