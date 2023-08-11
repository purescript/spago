module Web.HTML.HTMLTableHeaderCellElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLTableCellElement (HTMLTableCellElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTableHeaderCellElement :: Type

fromHTMLTableCellElement :: HTMLTableCellElement -> Maybe HTMLTableHeaderCellElement
fromHTMLTableCellElement = unsafeReadProtoTagged "HTMLTableHeaderCellElement"

fromHTMLElement :: HTMLElement -> Maybe HTMLTableHeaderCellElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTableHeaderCellElement"

fromElement :: Element -> Maybe HTMLTableHeaderCellElement
fromElement = unsafeReadProtoTagged "HTMLTableHeaderCellElement"

fromNode :: Node -> Maybe HTMLTableHeaderCellElement
fromNode = unsafeReadProtoTagged "HTMLTableHeaderCellElement"

fromChildNode :: ChildNode -> Maybe HTMLTableHeaderCellElement
fromChildNode = unsafeReadProtoTagged "HTMLTableHeaderCellElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTableHeaderCellElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTableHeaderCellElement"

fromParentNode :: ParentNode -> Maybe HTMLTableHeaderCellElement
fromParentNode = unsafeReadProtoTagged "HTMLTableHeaderCellElement"

fromEventTarget :: EventTarget -> Maybe HTMLTableHeaderCellElement
fromEventTarget = unsafeReadProtoTagged "HTMLTableHeaderCellElement"

toHTMLTableCellElement :: HTMLTableHeaderCellElement -> HTMLTableCellElement
toHTMLTableCellElement = unsafeCoerce

toHTMLElement :: HTMLTableHeaderCellElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTableHeaderCellElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTableHeaderCellElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTableHeaderCellElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTableHeaderCellElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTableHeaderCellElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTableHeaderCellElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import scope :: HTMLTableHeaderCellElement -> Effect String
foreign import setScope :: String -> HTMLTableHeaderCellElement -> Effect Unit

foreign import abbr :: HTMLTableHeaderCellElement -> Effect String
foreign import setAbbr :: String -> HTMLTableHeaderCellElement -> Effect Unit
