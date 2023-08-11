module Web.HTML.HTMLOListElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLOListElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLOListElement
fromHTMLElement = unsafeReadProtoTagged "HTMLOListElement"

fromElement :: Element -> Maybe HTMLOListElement
fromElement = unsafeReadProtoTagged "HTMLOListElement"

fromNode :: Node -> Maybe HTMLOListElement
fromNode = unsafeReadProtoTagged "HTMLOListElement"

fromChildNode :: ChildNode -> Maybe HTMLOListElement
fromChildNode = unsafeReadProtoTagged "HTMLOListElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLOListElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLOListElement"

fromParentNode :: ParentNode -> Maybe HTMLOListElement
fromParentNode = unsafeReadProtoTagged "HTMLOListElement"

fromEventTarget :: EventTarget -> Maybe HTMLOListElement
fromEventTarget = unsafeReadProtoTagged "HTMLOListElement"

toHTMLElement :: HTMLOListElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLOListElement -> Element
toElement = unsafeCoerce

toNode :: HTMLOListElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLOListElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLOListElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLOListElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLOListElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import reversed :: HTMLOListElement -> Effect Boolean
foreign import setReversed :: Boolean -> HTMLOListElement -> Effect Unit

foreign import start :: HTMLOListElement -> Effect Int
foreign import setStart :: Int -> HTMLOListElement -> Effect Unit

foreign import type_ :: HTMLOListElement -> Effect String
foreign import setType :: String -> HTMLOListElement -> Effect Unit
