module Web.HTML.HTMLModElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLModElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLModElement
fromHTMLElement = unsafeReadProtoTagged "HTMLModElement"

fromElement :: Element -> Maybe HTMLModElement
fromElement = unsafeReadProtoTagged "HTMLModElement"

fromNode :: Node -> Maybe HTMLModElement
fromNode = unsafeReadProtoTagged "HTMLModElement"

fromChildNode :: ChildNode -> Maybe HTMLModElement
fromChildNode = unsafeReadProtoTagged "HTMLModElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLModElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLModElement"

fromParentNode :: ParentNode -> Maybe HTMLModElement
fromParentNode = unsafeReadProtoTagged "HTMLModElement"

fromEventTarget :: EventTarget -> Maybe HTMLModElement
fromEventTarget = unsafeReadProtoTagged "HTMLModElement"

toHTMLElement :: HTMLModElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLModElement -> Element
toElement = unsafeCoerce

toNode :: HTMLModElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLModElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLModElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLModElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLModElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import cite :: HTMLModElement -> Effect String
foreign import setCite :: String -> HTMLModElement -> Effect Unit

foreign import dateTime :: HTMLModElement -> Effect String
foreign import setDateTime :: String -> HTMLModElement -> Effect Unit
