module Web.HTML.HTMLQuoteElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLQuoteElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLQuoteElement
fromHTMLElement = unsafeReadProtoTagged "HTMLQuoteElement"

fromElement :: Element -> Maybe HTMLQuoteElement
fromElement = unsafeReadProtoTagged "HTMLQuoteElement"

fromNode :: Node -> Maybe HTMLQuoteElement
fromNode = unsafeReadProtoTagged "HTMLQuoteElement"

fromChildNode :: ChildNode -> Maybe HTMLQuoteElement
fromChildNode = unsafeReadProtoTagged "HTMLQuoteElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLQuoteElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLQuoteElement"

fromParentNode :: ParentNode -> Maybe HTMLQuoteElement
fromParentNode = unsafeReadProtoTagged "HTMLQuoteElement"

fromEventTarget :: EventTarget -> Maybe HTMLQuoteElement
fromEventTarget = unsafeReadProtoTagged "HTMLQuoteElement"

toHTMLElement :: HTMLQuoteElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLQuoteElement -> Element
toElement = unsafeCoerce

toNode :: HTMLQuoteElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLQuoteElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLQuoteElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLQuoteElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLQuoteElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import cite :: HTMLQuoteElement -> Effect Boolean
foreign import setCite :: Boolean -> HTMLQuoteElement -> Effect Unit
