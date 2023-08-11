module Web.HTML.HTMLOutputElement
  ( HTMLOutputElement
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
  , form
  , name
  , setName
  , type_
  , defaultValue
  , setDefaultValue
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
import Web.DOM.NodeList (NodeList)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLFormElement (HTMLFormElement)
import Web.HTML.ValidityState (ValidityState)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLOutputElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLOutputElement
fromHTMLElement = unsafeReadProtoTagged "HTMLOutputElement"

fromElement :: Element -> Maybe HTMLOutputElement
fromElement = unsafeReadProtoTagged "HTMLOutputElement"

fromNode :: Node -> Maybe HTMLOutputElement
fromNode = unsafeReadProtoTagged "HTMLOutputElement"

fromChildNode :: ChildNode -> Maybe HTMLOutputElement
fromChildNode = unsafeReadProtoTagged "HTMLOutputElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLOutputElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLOutputElement"

fromParentNode :: ParentNode -> Maybe HTMLOutputElement
fromParentNode = unsafeReadProtoTagged "HTMLOutputElement"

fromEventTarget :: EventTarget -> Maybe HTMLOutputElement
fromEventTarget = unsafeReadProtoTagged "HTMLOutputElement"

toHTMLElement :: HTMLOutputElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLOutputElement -> Element
toElement = unsafeCoerce

toNode :: HTMLOutputElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLOutputElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLOutputElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLOutputElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLOutputElement -> EventTarget
toEventTarget = unsafeCoerce

--   [PutForwards=value] readonly attribute DOMSettableTokenList htmlFor;

form :: HTMLOutputElement -> Effect (Maybe HTMLFormElement)
form = map toMaybe <<< _form

foreign import _form :: HTMLOutputElement -> Effect (Nullable HTMLFormElement)

foreign import name :: HTMLOutputElement -> Effect String
foreign import setName :: String -> HTMLOutputElement -> Effect Unit

foreign import type_ :: HTMLOutputElement -> Effect String

foreign import defaultValue :: HTMLOutputElement -> Effect String
foreign import setDefaultValue :: String -> HTMLOutputElement -> Effect Unit

foreign import value :: HTMLOutputElement -> Effect String
foreign import setValue :: String -> HTMLOutputElement -> Effect Unit

foreign import willValidate :: HTMLOutputElement -> Effect Boolean

foreign import validity :: HTMLOutputElement -> Effect ValidityState

foreign import validationMessage :: HTMLOutputElement -> Effect String

foreign import checkValidity :: HTMLOutputElement -> Effect Boolean

foreign import reportValidity :: HTMLOutputElement -> Effect Boolean

foreign import setCustomValidity :: String -> HTMLOutputElement -> Effect Unit

foreign import labels :: HTMLOutputElement -> Effect NodeList
