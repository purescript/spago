module Web.HTML.HTMLTableCellElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTableCellElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLTableCellElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTableCellElement"

fromElement :: Element -> Maybe HTMLTableCellElement
fromElement = unsafeReadProtoTagged "HTMLTableCellElement"

fromNode :: Node -> Maybe HTMLTableCellElement
fromNode = unsafeReadProtoTagged "HTMLTableCellElement"

fromChildNode :: ChildNode -> Maybe HTMLTableCellElement
fromChildNode = unsafeReadProtoTagged "HTMLTableCellElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTableCellElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTableCellElement"

fromParentNode :: ParentNode -> Maybe HTMLTableCellElement
fromParentNode = unsafeReadProtoTagged "HTMLTableCellElement"

fromEventTarget :: EventTarget -> Maybe HTMLTableCellElement
fromEventTarget = unsafeReadProtoTagged "HTMLTableCellElement"

toHTMLElement :: HTMLTableCellElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTableCellElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTableCellElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTableCellElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTableCellElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTableCellElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTableCellElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import colSpan :: HTMLTableCellElement -> Effect Int
foreign import setColSpan :: Int -> HTMLTableCellElement -> Effect Unit

foreign import rowSpan :: HTMLTableCellElement -> Effect Int
foreign import setRowSpan :: Int -> HTMLTableCellElement -> Effect Unit

--   [PutForwards=value] readonly attribute DOMSettableTokenList headers;

foreign import cellIndex :: HTMLTableCellElement -> Effect Int
