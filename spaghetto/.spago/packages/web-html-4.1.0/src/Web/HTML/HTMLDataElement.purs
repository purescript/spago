module Web.HTML.HTMLDataElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLDataElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLDataElement
fromHTMLElement = unsafeReadProtoTagged "HTMLDataElement"

fromElement :: Element -> Maybe HTMLDataElement
fromElement = unsafeReadProtoTagged "HTMLDataElement"

fromNode :: Node -> Maybe HTMLDataElement
fromNode = unsafeReadProtoTagged "HTMLDataElement"

fromChildNode :: ChildNode -> Maybe HTMLDataElement
fromChildNode = unsafeReadProtoTagged "HTMLDataElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLDataElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLDataElement"

fromParentNode :: ParentNode -> Maybe HTMLDataElement
fromParentNode = unsafeReadProtoTagged "HTMLDataElement"

fromEventTarget :: EventTarget -> Maybe HTMLDataElement
fromEventTarget = unsafeReadProtoTagged "HTMLDataElement"

toHTMLElement :: HTMLDataElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLDataElement -> Element
toElement = unsafeCoerce

toNode :: HTMLDataElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLDataElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLDataElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLDataElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLDataElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import value :: HTMLDataElement -> Effect String
foreign import setValue :: String -> HTMLDataElement -> Effect Unit
