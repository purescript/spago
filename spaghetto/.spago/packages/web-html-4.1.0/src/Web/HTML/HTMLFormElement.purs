module Web.HTML.HTMLFormElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLFormElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLFormElement
fromHTMLElement = unsafeReadProtoTagged "HTMLFormElement"

fromElement :: Element -> Maybe HTMLFormElement
fromElement = unsafeReadProtoTagged "HTMLFormElement"

fromNode :: Node -> Maybe HTMLFormElement
fromNode = unsafeReadProtoTagged "HTMLFormElement"

fromChildNode :: ChildNode -> Maybe HTMLFormElement
fromChildNode = unsafeReadProtoTagged "HTMLFormElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLFormElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLFormElement"

fromParentNode :: ParentNode -> Maybe HTMLFormElement
fromParentNode = unsafeReadProtoTagged "HTMLFormElement"

fromEventTarget :: EventTarget -> Maybe HTMLFormElement
fromEventTarget = unsafeReadProtoTagged "HTMLFormElement"

toHTMLElement :: HTMLFormElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLFormElement -> Element
toElement = unsafeCoerce

toNode :: HTMLFormElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLFormElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLFormElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLFormElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLFormElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import acceptCharset :: HTMLFormElement -> Effect String
foreign import setAcceptCharset :: String -> HTMLFormElement -> Effect Unit

foreign import action :: HTMLFormElement -> Effect String
foreign import setAction :: String -> HTMLFormElement -> Effect Unit

foreign import autocomplete :: HTMLFormElement -> Effect String
foreign import setAutocomplete :: String -> HTMLFormElement -> Effect Unit

foreign import enctype :: HTMLFormElement -> Effect String
foreign import setEnctype :: String -> HTMLFormElement -> Effect Unit

foreign import encoding :: HTMLFormElement -> Effect String
foreign import setEncoding :: String -> HTMLFormElement -> Effect Unit

foreign import method :: HTMLFormElement -> Effect String
foreign import setMethod :: String -> HTMLFormElement -> Effect Unit

foreign import name :: HTMLFormElement -> Effect String
foreign import setName :: String -> HTMLFormElement -> Effect Unit

foreign import noValidate :: HTMLFormElement -> Effect Boolean
foreign import setNoValidate :: Boolean -> HTMLFormElement -> Effect Unit

foreign import target :: HTMLFormElement -> Effect String
foreign import setTarget :: String -> HTMLFormElement -> Effect Unit

--   readonly attribute HTMLFormControlsCollection elements;

foreign import length :: HTMLFormElement -> Effect Int

--   getter Element (unsigned long index);
--   getter (RadioNodeList or Element) (DOMString name);

foreign import submit :: HTMLFormElement -> Effect Unit
foreign import reset :: HTMLFormElement -> Effect Unit
foreign import checkValidity :: HTMLFormElement -> Effect Boolean
foreign import reportValidity :: HTMLFormElement -> Effect Boolean
