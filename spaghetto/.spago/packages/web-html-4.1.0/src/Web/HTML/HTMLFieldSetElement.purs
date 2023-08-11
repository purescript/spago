module Web.HTML.HTMLFieldSetElement
  ( HTMLFieldSetElement
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
  , name
  , setName
  , type_
  , setType
  , willValidate
  , validity
  , validationMessage
  , checkValidity
  , reportValidity
  , setCustomValidity
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
import Web.HTML.ValidityState (ValidityState)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLFieldSetElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLFieldSetElement
fromHTMLElement = unsafeReadProtoTagged "HTMLFieldSetElement"

fromElement :: Element -> Maybe HTMLFieldSetElement
fromElement = unsafeReadProtoTagged "HTMLFieldSetElement"

fromNode :: Node -> Maybe HTMLFieldSetElement
fromNode = unsafeReadProtoTagged "HTMLFieldSetElement"

fromChildNode :: ChildNode -> Maybe HTMLFieldSetElement
fromChildNode = unsafeReadProtoTagged "HTMLFieldSetElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLFieldSetElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLFieldSetElement"

fromParentNode :: ParentNode -> Maybe HTMLFieldSetElement
fromParentNode = unsafeReadProtoTagged "HTMLFieldSetElement"

fromEventTarget :: EventTarget -> Maybe HTMLFieldSetElement
fromEventTarget = unsafeReadProtoTagged "HTMLFieldSetElement"

toHTMLElement :: HTMLFieldSetElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLFieldSetElement -> Element
toElement = unsafeCoerce

toNode :: HTMLFieldSetElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLFieldSetElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLFieldSetElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLFieldSetElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLFieldSetElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import disabled :: HTMLFieldSetElement -> Effect Boolean
foreign import setDisabled :: Boolean -> HTMLFieldSetElement -> Effect Unit

foreign import _form :: HTMLFieldSetElement -> Effect (Nullable HTMLFormElement)

form :: HTMLFieldSetElement -> Effect (Maybe HTMLFormElement)
form = map toMaybe <<< _form

foreign import name :: HTMLFieldSetElement -> Effect String
foreign import setName :: String -> HTMLFieldSetElement -> Effect Unit

foreign import type_ :: HTMLFieldSetElement -> Effect String
foreign import setType :: String -> HTMLFieldSetElement -> Effect Unit

--   readonly attribute HTMLFormControlsCollection elements;

foreign import willValidate :: HTMLFieldSetElement -> Effect Boolean

foreign import validity :: HTMLFieldSetElement -> Effect ValidityState

foreign import validationMessage :: HTMLFieldSetElement -> Effect String

foreign import checkValidity :: HTMLFieldSetElement -> Effect Boolean

foreign import reportValidity :: HTMLFieldSetElement -> Effect Boolean

foreign import setCustomValidity :: String -> HTMLFieldSetElement -> Effect Unit
