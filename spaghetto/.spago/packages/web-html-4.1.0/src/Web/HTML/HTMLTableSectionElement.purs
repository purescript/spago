module Web.HTML.HTMLTableSectionElement
  ( HTMLTableSectionElement
  , fromHTMLElement
  , fromElement
  , fromNode
  , fromChildNode
  , fromNonDocumentTypeChildNode
  , fromParentNode
  , fromEventTarget
  , toHTMLElement
  , toElement
  , toNode
  , toChildNode
  , toNonDocumentTypeChildNode
  , toParentNode
  , toEventTarget
  , rows
  , insertRow
  , insertRow'
  , deleteRow
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.HTMLCollection (HTMLCollection)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTableSectionElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLTableSectionElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTableSectionElement"

fromElement :: Element -> Maybe HTMLTableSectionElement
fromElement = unsafeReadProtoTagged "HTMLTableSectionElement"

fromNode :: Node -> Maybe HTMLTableSectionElement
fromNode = unsafeReadProtoTagged "HTMLTableSectionElement"

fromChildNode :: ChildNode -> Maybe HTMLTableSectionElement
fromChildNode = unsafeReadProtoTagged "HTMLTableSectionElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTableSectionElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTableSectionElement"

fromParentNode :: ParentNode -> Maybe HTMLTableSectionElement
fromParentNode = unsafeReadProtoTagged "HTMLTableSectionElement"

fromEventTarget :: EventTarget -> Maybe HTMLTableSectionElement
fromEventTarget = unsafeReadProtoTagged "HTMLTableSectionElement"

toHTMLElement :: HTMLTableSectionElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTableSectionElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTableSectionElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTableSectionElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTableSectionElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTableSectionElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTableSectionElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import rows :: HTMLTableSectionElement -> Effect HTMLCollection

insertRow :: HTMLTableSectionElement -> Effect HTMLElement
insertRow = insertRow' (-1)

foreign import insertRowAt :: Int -> HTMLTableSectionElement -> Effect HTMLElement

insertRow' :: Int -> HTMLTableSectionElement -> Effect HTMLElement
insertRow' = insertRowAt

foreign import deleteRow :: Int -> HTMLTableSectionElement -> Effect Unit
