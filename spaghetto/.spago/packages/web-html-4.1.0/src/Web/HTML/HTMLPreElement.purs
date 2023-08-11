module Web.HTML.HTMLPreElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLPreElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLPreElement
fromHTMLElement = unsafeReadProtoTagged "HTMLPreElement"

fromElement :: Element -> Maybe HTMLPreElement
fromElement = unsafeReadProtoTagged "HTMLPreElement"

fromNode :: Node -> Maybe HTMLPreElement
fromNode = unsafeReadProtoTagged "HTMLPreElement"

fromChildNode :: ChildNode -> Maybe HTMLPreElement
fromChildNode = unsafeReadProtoTagged "HTMLPreElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLPreElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLPreElement"

fromParentNode :: ParentNode -> Maybe HTMLPreElement
fromParentNode = unsafeReadProtoTagged "HTMLPreElement"

fromEventTarget :: EventTarget -> Maybe HTMLPreElement
fromEventTarget = unsafeReadProtoTagged "HTMLPreElement"

toHTMLElement :: HTMLPreElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLPreElement -> Element
toElement = unsafeCoerce

toNode :: HTMLPreElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLPreElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLPreElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLPreElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLPreElement -> EventTarget
toEventTarget = unsafeCoerce
