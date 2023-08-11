module Web.HTML.HTMLMapElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.HTMLCollection (HTMLCollection)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLMapElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLMapElement
fromHTMLElement = unsafeReadProtoTagged "HTMLMapElement"

fromElement :: Element -> Maybe HTMLMapElement
fromElement = unsafeReadProtoTagged "HTMLMapElement"

fromNode :: Node -> Maybe HTMLMapElement
fromNode = unsafeReadProtoTagged "HTMLMapElement"

fromChildNode :: ChildNode -> Maybe HTMLMapElement
fromChildNode = unsafeReadProtoTagged "HTMLMapElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLMapElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLMapElement"

fromParentNode :: ParentNode -> Maybe HTMLMapElement
fromParentNode = unsafeReadProtoTagged "HTMLMapElement"

fromEventTarget :: EventTarget -> Maybe HTMLMapElement
fromEventTarget = unsafeReadProtoTagged "HTMLMapElement"

toHTMLElement :: HTMLMapElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLMapElement -> Element
toElement = unsafeCoerce

toNode :: HTMLMapElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLMapElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLMapElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLMapElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLMapElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import name :: HTMLMapElement -> Effect String
foreign import setName :: String -> HTMLMapElement -> Effect Unit

foreign import areas :: HTMLMapElement -> Effect HTMLCollection

foreign import images :: HTMLMapElement -> Effect HTMLCollection
