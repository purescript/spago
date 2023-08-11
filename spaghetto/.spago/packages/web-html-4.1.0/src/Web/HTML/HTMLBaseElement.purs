module Web.HTML.HTMLBaseElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLBaseElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLBaseElement
fromHTMLElement = unsafeReadProtoTagged "HTMLBaseElement"

fromElement :: Element -> Maybe HTMLBaseElement
fromElement = unsafeReadProtoTagged "HTMLBaseElement"

fromNode :: Node -> Maybe HTMLBaseElement
fromNode = unsafeReadProtoTagged "HTMLBaseElement"

fromChildNode :: ChildNode -> Maybe HTMLBaseElement
fromChildNode = unsafeReadProtoTagged "HTMLBaseElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLBaseElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLBaseElement"

fromParentNode :: ParentNode -> Maybe HTMLBaseElement
fromParentNode = unsafeReadProtoTagged "HTMLBaseElement"

fromEventTarget :: EventTarget -> Maybe HTMLBaseElement
fromEventTarget = unsafeReadProtoTagged "HTMLBaseElement"

toHTMLElement :: HTMLBaseElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLBaseElement -> Element
toElement = unsafeCoerce

toNode :: HTMLBaseElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLBaseElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLBaseElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLBaseElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLBaseElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import href :: HTMLBaseElement -> Effect String
foreign import setHref :: String -> HTMLBaseElement -> Effect Unit

foreign import target :: HTMLBaseElement -> Effect String
foreign import setTarget :: String -> HTMLBaseElement -> Effect Unit
