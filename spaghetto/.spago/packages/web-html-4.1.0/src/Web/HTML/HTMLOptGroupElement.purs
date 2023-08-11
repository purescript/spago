module Web.HTML.HTMLOptGroupElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLOptGroupElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLOptGroupElement
fromHTMLElement = unsafeReadProtoTagged "HTMLOptGroupElement"

fromElement :: Element -> Maybe HTMLOptGroupElement
fromElement = unsafeReadProtoTagged "HTMLOptGroupElement"

fromNode :: Node -> Maybe HTMLOptGroupElement
fromNode = unsafeReadProtoTagged "HTMLOptGroupElement"

fromChildNode :: ChildNode -> Maybe HTMLOptGroupElement
fromChildNode = unsafeReadProtoTagged "HTMLOptGroupElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLOptGroupElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLOptGroupElement"

fromParentNode :: ParentNode -> Maybe HTMLOptGroupElement
fromParentNode = unsafeReadProtoTagged "HTMLOptGroupElement"

fromEventTarget :: EventTarget -> Maybe HTMLOptGroupElement
fromEventTarget = unsafeReadProtoTagged "HTMLOptGroupElement"

toHTMLElement :: HTMLOptGroupElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLOptGroupElement -> Element
toElement = unsafeCoerce

toNode :: HTMLOptGroupElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLOptGroupElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLOptGroupElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLOptGroupElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLOptGroupElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import disabled :: HTMLOptGroupElement -> Effect Boolean
foreign import setDisabled :: Boolean -> HTMLOptGroupElement -> Effect Unit

foreign import label :: HTMLOptGroupElement -> Effect String
foreign import setLabel :: String -> HTMLOptGroupElement -> Effect Unit
