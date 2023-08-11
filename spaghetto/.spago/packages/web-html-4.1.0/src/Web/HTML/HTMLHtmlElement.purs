module Web.HTML.HTMLHtmlElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLHtmlElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLHtmlElement
fromHTMLElement = unsafeReadProtoTagged "HTMLHtmlElement"

fromElement :: Element -> Maybe HTMLHtmlElement
fromElement = unsafeReadProtoTagged "HTMLHtmlElement"

fromNode :: Node -> Maybe HTMLHtmlElement
fromNode = unsafeReadProtoTagged "HTMLHtmlElement"

fromChildNode :: ChildNode -> Maybe HTMLHtmlElement
fromChildNode = unsafeReadProtoTagged "HTMLHtmlElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLHtmlElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLHtmlElement"

fromParentNode :: ParentNode -> Maybe HTMLHtmlElement
fromParentNode = unsafeReadProtoTagged "HTMLHtmlElement"

fromEventTarget :: EventTarget -> Maybe HTMLHtmlElement
fromEventTarget = unsafeReadProtoTagged "HTMLHtmlElement"

toHTMLElement :: HTMLHtmlElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLHtmlElement -> Element
toElement = unsafeCoerce

toNode :: HTMLHtmlElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLHtmlElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLHtmlElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLHtmlElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLHtmlElement -> EventTarget
toEventTarget = unsafeCoerce
