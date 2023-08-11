module Web.HTML.HTMLAreaElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.DOMTokenList (DOMTokenList)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLAreaElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLAreaElement
fromHTMLElement = unsafeReadProtoTagged "HTMLAreaElement"

fromElement :: Element -> Maybe HTMLAreaElement
fromElement = unsafeReadProtoTagged "HTMLAreaElement"

fromNode :: Node -> Maybe HTMLAreaElement
fromNode = unsafeReadProtoTagged "HTMLAreaElement"

fromChildNode :: ChildNode -> Maybe HTMLAreaElement
fromChildNode = unsafeReadProtoTagged "HTMLAreaElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLAreaElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLAreaElement"

fromParentNode :: ParentNode -> Maybe HTMLAreaElement
fromParentNode = unsafeReadProtoTagged "HTMLAreaElement"

fromEventTarget :: EventTarget -> Maybe HTMLAreaElement
fromEventTarget = unsafeReadProtoTagged "HTMLAreaElement"

toHTMLElement :: HTMLAreaElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLAreaElement -> Element
toElement = unsafeCoerce

toNode :: HTMLAreaElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLAreaElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLAreaElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLAreaElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLAreaElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import alt :: HTMLAreaElement -> Effect String
foreign import setAlt :: String -> HTMLAreaElement -> Effect Unit

foreign import coords :: HTMLAreaElement -> Effect String
foreign import setCoords :: String -> HTMLAreaElement -> Effect Unit

foreign import shape :: HTMLAreaElement -> Effect String
foreign import setShape :: String -> HTMLAreaElement -> Effect Unit

foreign import target :: HTMLAreaElement -> Effect String
foreign import setTarget :: String -> HTMLAreaElement -> Effect Unit

foreign import download :: HTMLAreaElement -> Effect String
foreign import setDownload :: String -> HTMLAreaElement -> Effect Unit

foreign import rel :: HTMLAreaElement -> Effect String
foreign import setRel :: String -> HTMLAreaElement -> Effect Unit

foreign import relList :: HTMLAreaElement -> Effect DOMTokenList

foreign import hreflang :: HTMLAreaElement -> Effect String
foreign import setHreflang :: String -> HTMLAreaElement -> Effect Unit

foreign import type_ :: HTMLAreaElement -> Effect String
foreign import setType :: String -> HTMLAreaElement -> Effect Unit
