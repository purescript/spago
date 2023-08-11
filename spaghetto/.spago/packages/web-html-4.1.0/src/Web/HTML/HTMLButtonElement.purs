module Web.HTML.HTMLButtonElement
  ( HTMLButtonElement
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
  , formAction
  , setFormAction
  , formEnctype
  , setFormEnctype
  , formMethod
  , setFormMethod
  , formNoValidate
  , setFormNoValidate
  , formTarget
  , setFormTarget
  , name
  , setName
  , type_
  , setType
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

foreign import data HTMLButtonElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLButtonElement
fromHTMLElement = unsafeReadProtoTagged "HTMLButtonElement"

fromElement :: Element -> Maybe HTMLButtonElement
fromElement = unsafeReadProtoTagged "HTMLButtonElement"

fromNode :: Node -> Maybe HTMLButtonElement
fromNode = unsafeReadProtoTagged "HTMLButtonElement"

fromChildNode :: ChildNode -> Maybe HTMLButtonElement
fromChildNode = unsafeReadProtoTagged "HTMLButtonElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLButtonElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLButtonElement"

fromParentNode :: ParentNode -> Maybe HTMLButtonElement
fromParentNode = unsafeReadProtoTagged "HTMLButtonElement"

fromEventTarget :: EventTarget -> Maybe HTMLButtonElement
fromEventTarget = unsafeReadProtoTagged "HTMLButtonElement"

toHTMLElement :: HTMLButtonElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLButtonElement -> Element
toElement = unsafeCoerce

toNode :: HTMLButtonElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLButtonElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLButtonElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLButtonElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLButtonElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import autofocus :: HTMLButtonElement -> Effect Boolean
foreign import setAutofocus :: Boolean -> HTMLButtonElement -> Effect Unit

foreign import disabled :: HTMLButtonElement -> Effect Boolean
foreign import setDisabled :: Boolean -> HTMLButtonElement -> Effect Unit

form :: HTMLButtonElement -> Effect (Maybe HTMLFormElement)
form = map toMaybe <<< _form

foreign import _form :: HTMLButtonElement -> Effect (Nullable HTMLFormElement)

foreign import formAction :: HTMLButtonElement -> Effect String
foreign import setFormAction :: String -> HTMLButtonElement -> Effect Unit

foreign import formEnctype :: HTMLButtonElement -> Effect String
foreign import setFormEnctype :: String -> HTMLButtonElement -> Effect Unit

foreign import formMethod :: HTMLButtonElement -> Effect String
foreign import setFormMethod :: String -> HTMLButtonElement -> Effect Unit

foreign import formNoValidate :: HTMLButtonElement -> Effect Boolean
foreign import setFormNoValidate :: Boolean -> HTMLButtonElement -> Effect Unit

foreign import formTarget :: HTMLButtonElement -> Effect String
foreign import setFormTarget :: String -> HTMLButtonElement -> Effect Unit

foreign import name :: HTMLButtonElement -> Effect String
foreign import setName :: String -> HTMLButtonElement -> Effect Unit

foreign import type_ :: HTMLButtonElement -> Effect String
foreign import setType :: String -> HTMLButtonElement -> Effect Unit

foreign import value :: HTMLButtonElement -> Effect String
foreign import setValue :: String -> HTMLButtonElement -> Effect Unit

foreign import willValidate :: HTMLButtonElement -> Effect Boolean

foreign import validity :: HTMLButtonElement -> Effect ValidityState

foreign import validationMessage :: HTMLButtonElement -> Effect String

foreign import checkValidity :: HTMLButtonElement -> Effect Boolean

foreign import reportValidity :: HTMLButtonElement -> Effect Boolean

foreign import setCustomValidity :: String -> HTMLButtonElement -> Effect Unit

foreign import labels :: HTMLButtonElement -> Effect NodeList
