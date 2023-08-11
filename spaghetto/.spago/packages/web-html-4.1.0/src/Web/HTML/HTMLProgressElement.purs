module Web.HTML.HTMLProgressElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.NodeList (NodeList)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLProgressElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLProgressElement
fromHTMLElement = unsafeReadProtoTagged "HTMLProgressElement"

fromElement :: Element -> Maybe HTMLProgressElement
fromElement = unsafeReadProtoTagged "HTMLProgressElement"

fromNode :: Node -> Maybe HTMLProgressElement
fromNode = unsafeReadProtoTagged "HTMLProgressElement"

fromChildNode :: ChildNode -> Maybe HTMLProgressElement
fromChildNode = unsafeReadProtoTagged "HTMLProgressElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLProgressElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLProgressElement"

fromParentNode :: ParentNode -> Maybe HTMLProgressElement
fromParentNode = unsafeReadProtoTagged "HTMLProgressElement"

fromEventTarget :: EventTarget -> Maybe HTMLProgressElement
fromEventTarget = unsafeReadProtoTagged "HTMLProgressElement"

toHTMLElement :: HTMLProgressElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLProgressElement -> Element
toElement = unsafeCoerce

toNode :: HTMLProgressElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLProgressElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLProgressElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLProgressElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLProgressElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import value :: HTMLProgressElement -> Effect Number
foreign import setValue :: Number -> HTMLProgressElement -> Effect Unit

foreign import max :: HTMLProgressElement -> Effect Number
foreign import setMax :: Number -> HTMLProgressElement -> Effect Unit

foreign import position :: HTMLProgressElement -> Effect Number

foreign import labels :: HTMLProgressElement -> Effect NodeList
