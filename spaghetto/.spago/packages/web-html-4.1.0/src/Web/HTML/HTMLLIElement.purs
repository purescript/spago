module Web.HTML.HTMLLIElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLLIElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLLIElement
fromHTMLElement = unsafeReadProtoTagged "HTMLLIElement"

fromElement :: Element -> Maybe HTMLLIElement
fromElement = unsafeReadProtoTagged "HTMLLIElement"

fromNode :: Node -> Maybe HTMLLIElement
fromNode = unsafeReadProtoTagged "HTMLLIElement"

fromChildNode :: ChildNode -> Maybe HTMLLIElement
fromChildNode = unsafeReadProtoTagged "HTMLLIElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLLIElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLLIElement"

fromParentNode :: ParentNode -> Maybe HTMLLIElement
fromParentNode = unsafeReadProtoTagged "HTMLLIElement"

fromEventTarget :: EventTarget -> Maybe HTMLLIElement
fromEventTarget = unsafeReadProtoTagged "HTMLLIElement"

toHTMLElement :: HTMLLIElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLLIElement -> Element
toElement = unsafeCoerce

toNode :: HTMLLIElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLLIElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLLIElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLLIElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLLIElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import value :: HTMLLIElement -> Effect Int
foreign import setValue :: Int -> HTMLLIElement -> Effect Unit
