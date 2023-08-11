module Web.HTML.HTMLTableElement
  ( HTMLTableElement
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
  , caption
  , setCaption
  , createCaption
  , deleteCaption
  , tHead
  , setTHead
  , createTHead
  , deleteTHead
  , tFoot
  , setTFoot
  , createTFoot
  , deleteTFoot
  , tBodies
  , createTBody
  , rows
  , insertRow
  , insertRow'
  , deleteRow
  , border
  , setBorder
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.HTMLCollection (HTMLCollection)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLTableCaptionElement (HTMLTableCaptionElement)
import Web.HTML.HTMLTableSectionElement (HTMLTableSectionElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTableElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLTableElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTableElement"

fromElement :: Element -> Maybe HTMLTableElement
fromElement = unsafeReadProtoTagged "HTMLTableElement"

fromNode :: Node -> Maybe HTMLTableElement
fromNode = unsafeReadProtoTagged "HTMLTableElement"

fromChildNode :: ChildNode -> Maybe HTMLTableElement
fromChildNode = unsafeReadProtoTagged "HTMLTableElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTableElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTableElement"

fromParentNode :: ParentNode -> Maybe HTMLTableElement
fromParentNode = unsafeReadProtoTagged "HTMLTableElement"

fromEventTarget :: EventTarget -> Maybe HTMLTableElement
fromEventTarget = unsafeReadProtoTagged "HTMLTableElement"

toHTMLElement :: HTMLTableElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTableElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTableElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTableElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTableElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTableElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTableElement -> EventTarget
toEventTarget = unsafeCoerce

caption :: HTMLTableElement -> Effect (Maybe HTMLTableCaptionElement)
caption = map toMaybe <<< _caption

setCaption :: Maybe HTMLTableCaptionElement -> HTMLTableElement -> Effect Unit
setCaption = _setCaption <<< toNullable

foreign import _caption :: HTMLTableElement -> Effect (Nullable HTMLTableCaptionElement)
foreign import _setCaption :: Nullable HTMLTableCaptionElement -> HTMLTableElement -> Effect Unit

foreign import createCaption :: HTMLTableElement -> Effect HTMLElement
foreign import deleteCaption :: HTMLTableElement -> Effect Unit

tHead :: HTMLTableElement -> Effect (Maybe HTMLTableSectionElement)
tHead = map toMaybe <<< _tHead

setTHead :: Maybe HTMLTableSectionElement -> HTMLTableElement -> Effect Unit
setTHead = _setTHead <<< toNullable

foreign import _tHead :: HTMLTableElement -> Effect (Nullable HTMLTableSectionElement)
foreign import _setTHead :: Nullable HTMLTableSectionElement -> HTMLTableElement -> Effect Unit

foreign import createTHead :: HTMLTableElement -> Effect HTMLElement
foreign import deleteTHead :: HTMLTableElement -> Effect Unit

tFoot :: HTMLTableElement -> Effect (Maybe HTMLTableSectionElement)
tFoot = map toMaybe <<< _tFoot

setTFoot :: Maybe HTMLTableSectionElement -> HTMLTableElement -> Effect Unit
setTFoot = _setTFoot <<< toNullable

foreign import _tFoot :: HTMLTableElement -> Effect (Nullable HTMLTableSectionElement)
foreign import _setTFoot :: Nullable HTMLTableSectionElement -> HTMLTableElement -> Effect Unit

foreign import createTFoot :: HTMLTableElement -> Effect HTMLElement
foreign import deleteTFoot :: HTMLTableElement -> Effect Unit

foreign import tBodies :: HTMLTableElement -> Effect HTMLCollection
foreign import createTBody :: HTMLTableElement -> Effect HTMLElement

foreign import rows :: HTMLTableElement -> Effect HTMLCollection

insertRow :: HTMLTableElement -> Effect HTMLElement
insertRow = insertRow' (-1)

foreign import insertRowAt :: Int -> HTMLTableElement -> Effect HTMLElement

insertRow' :: Int -> HTMLTableElement -> Effect HTMLElement
insertRow' = insertRowAt

foreign import deleteRow :: Int -> HTMLTableElement -> Effect Unit

foreign import border :: HTMLTableElement -> Effect String
foreign import setBorder :: String -> HTMLTableElement -> Effect Unit
