module Web.HTML.HTMLLabelElement
  ( HTMLLabelElement
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
  , htmlFor
  , setHtmlFor
  , control
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

foreign import data HTMLLabelElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLLabelElement
fromHTMLElement = unsafeReadProtoTagged "HTMLLabelElement"

fromElement :: Element -> Maybe HTMLLabelElement
fromElement = unsafeReadProtoTagged "HTMLLabelElement"

fromNode :: Node -> Maybe HTMLLabelElement
fromNode = unsafeReadProtoTagged "HTMLLabelElement"

fromChildNode :: ChildNode -> Maybe HTMLLabelElement
fromChildNode = unsafeReadProtoTagged "HTMLLabelElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLLabelElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLLabelElement"

fromParentNode :: ParentNode -> Maybe HTMLLabelElement
fromParentNode = unsafeReadProtoTagged "HTMLLabelElement"

fromEventTarget :: EventTarget -> Maybe HTMLLabelElement
fromEventTarget = unsafeReadProtoTagged "HTMLLabelElement"

toHTMLElement :: HTMLLabelElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLLabelElement -> Element
toElement = unsafeCoerce

toNode :: HTMLLabelElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLLabelElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLLabelElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLLabelElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLLabelElement -> EventTarget
toEventTarget = unsafeCoerce

form :: HTMLLabelElement -> Effect (Maybe HTMLFormElement)
form = map toMaybe <<< _form

foreign import _form :: HTMLLabelElement -> Effect (Nullable HTMLFormElement)

foreign import htmlFor :: HTMLLabelElement -> Effect String
foreign import setHtmlFor :: String -> HTMLLabelElement -> Effect Unit

control :: HTMLLabelElement -> Effect (Maybe HTMLElement)
control = map toMaybe <<< _control

foreign import _control :: HTMLLabelElement -> Effect (Nullable HTMLElement)
