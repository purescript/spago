module Web.HTML.HTMLSelectElement
  ( HTMLSelectElement
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
  , autofocus
  , setAutofocus
  , disabled
  , setDisabled
  , form
  , multiple
  , setMultiple
  , name
  , setName
  , required
  , setRequired
  , size
  , setSize
  , type_
  , length
  , setLength
  , selectedOptions
  , selectedIndex
  , setSelectedIndex
  , value
  , setValue
  , willValidate
  , validity
  , validationMessage
  , checkValidity
  , reportValidity
  , setCustomValidity
  , labels
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.HTMLCollection (HTMLCollection)
import Web.DOM.NodeList (NodeList)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLFormElement (HTMLFormElement)
import Web.HTML.ValidityState (ValidityState)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLSelectElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLSelectElement
fromHTMLElement = unsafeReadProtoTagged "HTMLSelectElement"

fromElement :: Element -> Maybe HTMLSelectElement
fromElement = unsafeReadProtoTagged "HTMLSelectElement"

fromNode :: Node -> Maybe HTMLSelectElement
fromNode = unsafeReadProtoTagged "HTMLSelectElement"

fromChildNode :: ChildNode -> Maybe HTMLSelectElement
fromChildNode = unsafeReadProtoTagged "HTMLSelectElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLSelectElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLSelectElement"

fromParentNode :: ParentNode -> Maybe HTMLSelectElement
fromParentNode = unsafeReadProtoTagged "HTMLSelectElement"

fromEventTarget :: EventTarget -> Maybe HTMLSelectElement
fromEventTarget = unsafeReadProtoTagged "HTMLSelectElement"

toHTMLElement :: HTMLSelectElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLSelectElement -> Element
toElement = unsafeCoerce

toNode :: HTMLSelectElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLSelectElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLSelectElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLSelectElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLSelectElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import autofocus :: HTMLSelectElement -> Effect Boolean
foreign import setAutofocus :: Boolean -> HTMLSelectElement -> Effect Unit

foreign import disabled :: HTMLSelectElement -> Effect Boolean
foreign import setDisabled :: Boolean -> HTMLSelectElement -> Effect Unit

form :: HTMLSelectElement -> Effect (Maybe HTMLFormElement)
form = map toMaybe <<< _form

foreign import _form :: HTMLSelectElement -> Effect (Nullable HTMLFormElement)

foreign import multiple :: HTMLSelectElement -> Effect Boolean
foreign import setMultiple :: Boolean -> HTMLSelectElement -> Effect Unit

foreign import name :: HTMLSelectElement -> Effect String
foreign import setName :: String -> HTMLSelectElement -> Effect Unit

foreign import required :: HTMLSelectElement -> Effect Boolean
foreign import setRequired :: Boolean -> HTMLSelectElement -> Effect Unit

foreign import size :: HTMLSelectElement -> Effect Int
foreign import setSize :: Int -> HTMLSelectElement -> Effect Unit

foreign import type_ :: HTMLSelectElement -> Effect String

--   readonly attribute HTMLOptionsCollection options;

foreign import length :: HTMLSelectElement -> Effect Int
foreign import setLength :: Int -> HTMLSelectElement -> Effect Unit

--   getter Element? item(unsigned long index);
--   HTMLOptionElement? namedItem(DOMString name);
--   void add((HTMLOptionElement or HTMLOptGroupElement) element, optional (HTMLElement or long)? before = null);
--   void remove(); // ChildNode overload
--   void remove(long index);
--   setter creator void (unsigned long index, HTMLOptionElement? option);

foreign import selectedOptions :: HTMLSelectElement -> Effect HTMLCollection

foreign import selectedIndex :: HTMLSelectElement -> Effect Int
foreign import setSelectedIndex :: Int -> HTMLSelectElement -> Effect Unit

foreign import value :: HTMLSelectElement -> Effect String
foreign import setValue :: String -> HTMLSelectElement -> Effect Unit

foreign import willValidate :: HTMLSelectElement -> Effect Boolean

foreign import validity :: HTMLSelectElement -> Effect ValidityState

foreign import validationMessage :: HTMLSelectElement -> Effect String

foreign import checkValidity :: HTMLSelectElement -> Effect Boolean

foreign import reportValidity :: HTMLSelectElement -> Effect Boolean

foreign import setCustomValidity :: String -> HTMLSelectElement -> Effect Unit

foreign import labels :: HTMLSelectElement -> Effect NodeList
