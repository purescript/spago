module Web.HTML.HTMLOptionElement
  ( HTMLOptionElement
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
  , disabled
  , setDisabled
  , form
  , label
  , setLabel
  , defaultSelected
  , setDefaultSelected
  , selected
  , setSelected
  , value
  , setValue
  , text
  , setText
  , index
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLFormElement (HTMLFormElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLOptionElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLOptionElement
fromHTMLElement = unsafeReadProtoTagged "HTMLOptionElement"

fromElement :: Element -> Maybe HTMLOptionElement
fromElement = unsafeReadProtoTagged "HTMLOptionElement"

fromNode :: Node -> Maybe HTMLOptionElement
fromNode = unsafeReadProtoTagged "HTMLOptionElement"

fromChildNode :: ChildNode -> Maybe HTMLOptionElement
fromChildNode = unsafeReadProtoTagged "HTMLOptionElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLOptionElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLOptionElement"

fromParentNode :: ParentNode -> Maybe HTMLOptionElement
fromParentNode = unsafeReadProtoTagged "HTMLOptionElement"

fromEventTarget :: EventTarget -> Maybe HTMLOptionElement
fromEventTarget = unsafeReadProtoTagged "HTMLOptionElement"

toHTMLElement :: HTMLOptionElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLOptionElement -> Element
toElement = unsafeCoerce

toNode :: HTMLOptionElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLOptionElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLOptionElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLOptionElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLOptionElement -> EventTarget
toEventTarget = unsafeCoerce

-- [NamedConstructor=Option(optional DOMString text = "", optional DOMString value, optional boolean defaultSelected = false, optional boolean selected = false)]

foreign import disabled :: HTMLOptionElement -> Effect Boolean
foreign import setDisabled :: Boolean -> HTMLOptionElement -> Effect Unit

form :: HTMLOptionElement -> Effect (Maybe HTMLFormElement)
form = map toMaybe <<< _form

foreign import _form :: HTMLOptionElement -> Effect (Nullable HTMLFormElement)

foreign import label :: HTMLOptionElement -> Effect String
foreign import setLabel :: String -> HTMLOptionElement -> Effect Unit

foreign import defaultSelected :: HTMLOptionElement -> Effect Boolean
foreign import setDefaultSelected :: Boolean -> HTMLOptionElement -> Effect Unit

foreign import selected :: HTMLOptionElement -> Effect Boolean
foreign import setSelected :: Boolean -> HTMLOptionElement -> Effect Unit

foreign import value :: HTMLOptionElement -> Effect String
foreign import setValue :: String -> HTMLOptionElement -> Effect Unit

foreign import text :: HTMLOptionElement -> Effect String
foreign import setText :: String -> HTMLOptionElement -> Effect Unit

foreign import index :: HTMLOptionElement -> Effect Int
