module Web.HTML.HTMLTableColElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTableColElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLTableColElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTableColElement"

fromElement :: Element -> Maybe HTMLTableColElement
fromElement = unsafeReadProtoTagged "HTMLTableColElement"

fromNode :: Node -> Maybe HTMLTableColElement
fromNode = unsafeReadProtoTagged "HTMLTableColElement"

fromChildNode :: ChildNode -> Maybe HTMLTableColElement
fromChildNode = unsafeReadProtoTagged "HTMLTableColElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTableColElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTableColElement"

fromParentNode :: ParentNode -> Maybe HTMLTableColElement
fromParentNode = unsafeReadProtoTagged "HTMLTableColElement"

fromEventTarget :: EventTarget -> Maybe HTMLTableColElement
fromEventTarget = unsafeReadProtoTagged "HTMLTableColElement"

toHTMLElement :: HTMLTableColElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTableColElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTableColElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTableColElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTableColElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTableColElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTableColElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import span :: HTMLTableColElement -> Effect Int
foreign import setSpan :: Int -> HTMLTableColElement -> Effect Unit
