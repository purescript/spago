module Web.HTML.HTMLTitleElement where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTitleElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLTitleElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTitleElement"

fromElement :: Element -> Maybe HTMLTitleElement
fromElement = unsafeReadProtoTagged "HTMLTitleElement"

fromNode :: Node -> Maybe HTMLTitleElement
fromNode = unsafeReadProtoTagged "HTMLTitleElement"

fromChildNode :: ChildNode -> Maybe HTMLTitleElement
fromChildNode = unsafeReadProtoTagged "HTMLTitleElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTitleElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTitleElement"

fromParentNode :: ParentNode -> Maybe HTMLTitleElement
fromParentNode = unsafeReadProtoTagged "HTMLTitleElement"

fromEventTarget :: EventTarget -> Maybe HTMLTitleElement
fromEventTarget = unsafeReadProtoTagged "HTMLTitleElement"

toHTMLElement :: HTMLTitleElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTitleElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTitleElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTitleElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTitleElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTitleElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTitleElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import text :: HTMLTitleElement -> Effect String
foreign import setText :: String -> HTMLTitleElement -> Effect Unit
