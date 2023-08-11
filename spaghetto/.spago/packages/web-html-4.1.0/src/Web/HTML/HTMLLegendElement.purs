module Web.HTML.HTMLLegendElement
  ( HTMLLegendElement
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

foreign import data HTMLLegendElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLLegendElement
fromHTMLElement = unsafeReadProtoTagged "HTMLLegendElement"

fromElement :: Element -> Maybe HTMLLegendElement
fromElement = unsafeReadProtoTagged "HTMLLegendElement"

fromNode :: Node -> Maybe HTMLLegendElement
fromNode = unsafeReadProtoTagged "HTMLLegendElement"

fromChildNode :: ChildNode -> Maybe HTMLLegendElement
fromChildNode = unsafeReadProtoTagged "HTMLLegendElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLLegendElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLLegendElement"

fromParentNode :: ParentNode -> Maybe HTMLLegendElement
fromParentNode = unsafeReadProtoTagged "HTMLLegendElement"

fromEventTarget :: EventTarget -> Maybe HTMLLegendElement
fromEventTarget = unsafeReadProtoTagged "HTMLLegendElement"

toHTMLElement :: HTMLLegendElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLLegendElement -> Element
toElement = unsafeCoerce

toNode :: HTMLLegendElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLLegendElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLLegendElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLLegendElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLLegendElement -> EventTarget
toEventTarget = unsafeCoerce

form :: HTMLLegendElement -> Effect (Maybe HTMLFormElement)
form = map toMaybe <<< _form

foreign import _form :: HTMLLegendElement -> Effect (Nullable HTMLFormElement)
