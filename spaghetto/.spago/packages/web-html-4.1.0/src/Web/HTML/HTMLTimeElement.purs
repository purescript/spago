module Web.HTML.HTMLTimeElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTimeElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLTimeElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTimeElement"

fromElement :: Element -> Maybe HTMLTimeElement
fromElement = unsafeReadProtoTagged "HTMLTimeElement"

fromNode :: Node -> Maybe HTMLTimeElement
fromNode = unsafeReadProtoTagged "HTMLTimeElement"

fromChildNode :: ChildNode -> Maybe HTMLTimeElement
fromChildNode = unsafeReadProtoTagged "HTMLTimeElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTimeElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTimeElement"

fromParentNode :: ParentNode -> Maybe HTMLTimeElement
fromParentNode = unsafeReadProtoTagged "HTMLTimeElement"

fromEventTarget :: EventTarget -> Maybe HTMLTimeElement
fromEventTarget = unsafeReadProtoTagged "HTMLTimeElement"

toHTMLElement :: HTMLTimeElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTimeElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTimeElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTimeElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTimeElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTimeElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTimeElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import dateTime :: HTMLTimeElement -> Effect String
foreign import setDateTime :: String -> HTMLTimeElement -> Effect Unit
