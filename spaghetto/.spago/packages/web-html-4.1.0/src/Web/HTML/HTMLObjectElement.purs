module Web.HTML.HTMLObjectElement
  ( HTMLObjectElement
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
  , data_
  , setData
  , type_
  , setType
  , typeMustMatch
  , name
  , setName
  , useMap
  , setUseMap
  , form
  , width
  , setWidth
  , height
  , setHeight
  , contentDocument
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
import Web.DOM.Document (Document)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLFormElement (HTMLFormElement)
import Web.HTML.ValidityState (ValidityState)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLObjectElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLObjectElement
fromHTMLElement = unsafeReadProtoTagged "HTMLObjectElement"

fromElement :: Element -> Maybe HTMLObjectElement
fromElement = unsafeReadProtoTagged "HTMLObjectElement"

fromNode :: Node -> Maybe HTMLObjectElement
fromNode = unsafeReadProtoTagged "HTMLObjectElement"

fromChildNode :: ChildNode -> Maybe HTMLObjectElement
fromChildNode = unsafeReadProtoTagged "HTMLObjectElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLObjectElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLObjectElement"

fromParentNode :: ParentNode -> Maybe HTMLObjectElement
fromParentNode = unsafeReadProtoTagged "HTMLObjectElement"

fromEventTarget :: EventTarget -> Maybe HTMLObjectElement
fromEventTarget = unsafeReadProtoTagged "HTMLObjectElement"

toHTMLElement :: HTMLObjectElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLObjectElement -> Element
toElement = unsafeCoerce

toNode :: HTMLObjectElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLObjectElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLObjectElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLObjectElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLObjectElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import data_ :: HTMLObjectElement -> Effect String
foreign import setData :: String -> HTMLObjectElement -> Effect Unit

foreign import type_ :: HTMLObjectElement -> Effect String
foreign import setType :: String -> HTMLObjectElement -> Effect Unit

foreign import typeMustMatch :: HTMLObjectElement -> Effect Boolean

foreign import name :: HTMLObjectElement -> Effect String
foreign import setName :: String -> HTMLObjectElement -> Effect Unit

foreign import useMap :: HTMLObjectElement -> Effect String
foreign import setUseMap :: String -> HTMLObjectElement -> Effect Unit

form :: HTMLObjectElement -> Effect (Maybe HTMLFormElement)
form = map toMaybe <<< _form

foreign import _form :: HTMLObjectElement -> Effect (Nullable HTMLFormElement)

foreign import width :: HTMLObjectElement -> Effect String
foreign import setWidth :: String -> HTMLObjectElement -> Effect Unit

foreign import height :: HTMLObjectElement -> Effect String
foreign import setHeight :: String -> HTMLObjectElement -> Effect Unit

contentDocument :: HTMLObjectElement -> Effect (Maybe Document)
contentDocument = map toMaybe <<< _contentDocument

foreign import _contentDocument :: HTMLObjectElement -> Effect (Nullable Document)

--   readonly attribute WindowProxy? contentWindow;

foreign import willValidate :: HTMLObjectElement -> Effect Boolean

foreign import validity :: HTMLObjectElement -> Effect ValidityState

foreign import validationMessage :: HTMLObjectElement -> Effect String

foreign import checkValidity :: HTMLObjectElement -> Effect Boolean

foreign import reportValidity :: HTMLObjectElement -> Effect Boolean

foreign import setCustomValidity :: String -> HTMLObjectElement -> Effect Unit

--   legacycaller any (any... arguments);
