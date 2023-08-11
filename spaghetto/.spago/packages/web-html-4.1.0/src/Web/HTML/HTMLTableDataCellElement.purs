module Web.HTML.HTMLTableDataCellElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLTableCellElement (HTMLTableCellElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTableDataCellElement :: Type

fromHTMLTableCellElement :: HTMLTableCellElement -> Maybe HTMLTableDataCellElement
fromHTMLTableCellElement = unsafeReadProtoTagged "HTMLTableDataCellElement"

fromHTMLElement :: HTMLElement -> Maybe HTMLTableDataCellElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTableDataCellElement"

fromElement :: Element -> Maybe HTMLTableDataCellElement
fromElement = unsafeReadProtoTagged "HTMLTableDataCellElement"

fromNode :: Node -> Maybe HTMLTableDataCellElement
fromNode = unsafeReadProtoTagged "HTMLTableDataCellElement"

fromChildNode :: ChildNode -> Maybe HTMLTableDataCellElement
fromChildNode = unsafeReadProtoTagged "HTMLTableDataCellElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTableDataCellElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTableDataCellElement"

fromParentNode :: ParentNode -> Maybe HTMLTableDataCellElement
fromParentNode = unsafeReadProtoTagged "HTMLTableDataCellElement"

fromEventTarget :: EventTarget -> Maybe HTMLTableDataCellElement
fromEventTarget = unsafeReadProtoTagged "HTMLTableDataCellElement"

toHTMLTableCellElement :: HTMLTableDataCellElement -> HTMLTableCellElement
toHTMLTableCellElement = unsafeCoerce

toHTMLElement :: HTMLTableDataCellElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTableDataCellElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTableDataCellElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTableDataCellElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTableDataCellElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTableDataCellElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTableDataCellElement -> EventTarget
toEventTarget = unsafeCoerce
