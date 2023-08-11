module Web.HTML.HTMLBRElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLBRElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLBRElement
fromHTMLElement = unsafeReadProtoTagged "HTMLBRElement"

fromElement :: Element -> Maybe HTMLBRElement
fromElement = unsafeReadProtoTagged "HTMLBRElement"

fromNode :: Node -> Maybe HTMLBRElement
fromNode = unsafeReadProtoTagged "HTMLBRElement"

fromChildNode :: ChildNode -> Maybe HTMLBRElement
fromChildNode = unsafeReadProtoTagged "HTMLBRElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLBRElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLBRElement"

fromParentNode :: ParentNode -> Maybe HTMLBRElement
fromParentNode = unsafeReadProtoTagged "HTMLBRElement"

fromEventTarget :: EventTarget -> Maybe HTMLBRElement
fromEventTarget = unsafeReadProtoTagged "HTMLBRElement"

toHTMLElement :: HTMLBRElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLBRElement -> Element
toElement = unsafeCoerce

toNode :: HTMLBRElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLBRElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLBRElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLBRElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLBRElement -> EventTarget
toEventTarget = unsafeCoerce
