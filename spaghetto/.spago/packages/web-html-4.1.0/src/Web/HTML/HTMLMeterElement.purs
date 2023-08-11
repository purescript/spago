module Web.HTML.HTMLMeterElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.NodeList (NodeList)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLMeterElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLMeterElement
fromHTMLElement = unsafeReadProtoTagged "HTMLMeterElement"

fromElement :: Element -> Maybe HTMLMeterElement
fromElement = unsafeReadProtoTagged "HTMLMeterElement"

fromNode :: Node -> Maybe HTMLMeterElement
fromNode = unsafeReadProtoTagged "HTMLMeterElement"

fromChildNode :: ChildNode -> Maybe HTMLMeterElement
fromChildNode = unsafeReadProtoTagged "HTMLMeterElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLMeterElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLMeterElement"

fromParentNode :: ParentNode -> Maybe HTMLMeterElement
fromParentNode = unsafeReadProtoTagged "HTMLMeterElement"

fromEventTarget :: EventTarget -> Maybe HTMLMeterElement
fromEventTarget = unsafeReadProtoTagged "HTMLMeterElement"

toHTMLElement :: HTMLMeterElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLMeterElement -> Element
toElement = unsafeCoerce

toNode :: HTMLMeterElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLMeterElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLMeterElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLMeterElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLMeterElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import value :: HTMLMeterElement -> Effect Number
foreign import setValue :: Number -> HTMLMeterElement -> Effect Unit

foreign import min :: HTMLMeterElement -> Effect Number
foreign import setMin :: Number -> HTMLMeterElement -> Effect Unit

foreign import max :: HTMLMeterElement -> Effect Number
foreign import setMax :: Number -> HTMLMeterElement -> Effect Unit

foreign import low :: HTMLMeterElement -> Effect Number
foreign import setLow :: Number -> HTMLMeterElement -> Effect Unit

foreign import high :: HTMLMeterElement -> Effect Number
foreign import setHigh :: Number -> HTMLMeterElement -> Effect Unit

foreign import optimum :: HTMLMeterElement -> Effect Number
foreign import setOptimum :: Number -> HTMLMeterElement -> Effect Unit

foreign import labels :: HTMLMeterElement -> Effect NodeList
