module Web.HTML.HTMLKeygenElement
  ( HTMLKeygenElement
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
  , challenge
  , setChallenge
  , disabled
  , setDisabled
  , form
  , keytype
  , setKeytype
  , name
  , setName
  , type_
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

foreign import data HTMLKeygenElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLKeygenElement
fromHTMLElement = unsafeReadProtoTagged "HTMLKeygenElement"

fromElement :: Element -> Maybe HTMLKeygenElement
fromElement = unsafeReadProtoTagged "HTMLKeygenElement"

fromNode :: Node -> Maybe HTMLKeygenElement
fromNode = unsafeReadProtoTagged "HTMLKeygenElement"

fromChildNode :: ChildNode -> Maybe HTMLKeygenElement
fromChildNode = unsafeReadProtoTagged "HTMLKeygenElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLKeygenElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLKeygenElement"

fromParentNode :: ParentNode -> Maybe HTMLKeygenElement
fromParentNode = unsafeReadProtoTagged "HTMLKeygenElement"

fromEventTarget :: EventTarget -> Maybe HTMLKeygenElement
fromEventTarget = unsafeReadProtoTagged "HTMLKeygenElement"

toHTMLElement :: HTMLKeygenElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLKeygenElement -> Element
toElement = unsafeCoerce

toNode :: HTMLKeygenElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLKeygenElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLKeygenElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLKeygenElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLKeygenElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import autofocus :: HTMLKeygenElement -> Effect Boolean
foreign import setAutofocus :: Boolean -> HTMLKeygenElement -> Effect Unit

foreign import challenge :: HTMLKeygenElement -> Effect String
foreign import setChallenge :: String -> HTMLKeygenElement -> Effect Unit

foreign import disabled :: HTMLKeygenElement -> Effect Boolean
foreign import setDisabled :: Boolean -> HTMLKeygenElement -> Effect Unit

form :: HTMLKeygenElement -> Effect (Maybe HTMLFormElement)
form = map toMaybe <<< _form

foreign import _form :: HTMLKeygenElement -> Effect (Nullable HTMLFormElement)

foreign import keytype :: HTMLKeygenElement -> Effect String
foreign import setKeytype :: String -> HTMLKeygenElement -> Effect Unit

foreign import name :: HTMLKeygenElement -> Effect String
foreign import setName :: String -> HTMLKeygenElement -> Effect Unit

foreign import type_ :: HTMLKeygenElement -> Effect String

foreign import willValidate :: HTMLKeygenElement -> Effect Boolean

foreign import validity :: HTMLKeygenElement -> Effect ValidityState

foreign import validationMessage :: HTMLKeygenElement -> Effect String

foreign import checkValidity :: HTMLKeygenElement -> Effect Boolean

foreign import reportValidity :: HTMLKeygenElement -> Effect Boolean

foreign import setCustomValidity :: String -> HTMLKeygenElement -> Effect Unit

foreign import labels :: HTMLKeygenElement -> Effect NodeList
