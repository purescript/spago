module Web.HTML.HTMLLinkElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.DOMTokenList (DOMTokenList)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLLinkElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLLinkElement
fromHTMLElement = unsafeReadProtoTagged "HTMLLinkElement"

fromElement :: Element -> Maybe HTMLLinkElement
fromElement = unsafeReadProtoTagged "HTMLLinkElement"

fromNode :: Node -> Maybe HTMLLinkElement
fromNode = unsafeReadProtoTagged "HTMLLinkElement"

fromChildNode :: ChildNode -> Maybe HTMLLinkElement
fromChildNode = unsafeReadProtoTagged "HTMLLinkElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLLinkElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLLinkElement"

fromParentNode :: ParentNode -> Maybe HTMLLinkElement
fromParentNode = unsafeReadProtoTagged "HTMLLinkElement"

fromEventTarget :: EventTarget -> Maybe HTMLLinkElement
fromEventTarget = unsafeReadProtoTagged "HTMLLinkElement"

toHTMLElement :: HTMLLinkElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLLinkElement -> Element
toElement = unsafeCoerce

toNode :: HTMLLinkElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLLinkElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLLinkElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLLinkElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLLinkElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import disabled :: HTMLLinkElement -> Effect Boolean
foreign import setDisabled :: Boolean -> HTMLLinkElement -> Effect Unit

foreign import href :: HTMLLinkElement -> Effect String
foreign import setHref :: String -> HTMLLinkElement -> Effect Unit

foreign import crossOrigin :: HTMLLinkElement -> Effect String
foreign import setCrossOrigin :: String -> HTMLLinkElement -> Effect Unit

foreign import rel :: HTMLLinkElement -> Effect String
foreign import setRel :: String -> HTMLLinkElement -> Effect Unit

foreign import rev :: HTMLLinkElement -> Effect String
foreign import setRev :: String -> HTMLLinkElement -> Effect Unit

foreign import relList :: HTMLLinkElement -> Effect DOMTokenList

foreign import media :: HTMLLinkElement -> Effect String
foreign import setMedia :: String -> HTMLLinkElement -> Effect Unit

foreign import hreflang :: HTMLLinkElement -> Effect String
foreign import setHreflang :: String -> HTMLLinkElement -> Effect Unit

foreign import type_ :: HTMLLinkElement -> Effect String
foreign import setType :: String -> HTMLLinkElement -> Effect Unit

--   [PutForwards=value] readonly attribute DOMSettableTokenList sizes;

-- HTMLLinkElement implements LinkStyle;
