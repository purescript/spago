module Web.HTML.HTMLScriptElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLScriptElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLScriptElement
fromHTMLElement = unsafeReadProtoTagged "HTMLScriptElement"

fromElement :: Element -> Maybe HTMLScriptElement
fromElement = unsafeReadProtoTagged "HTMLScriptElement"

fromNode :: Node -> Maybe HTMLScriptElement
fromNode = unsafeReadProtoTagged "HTMLScriptElement"

fromChildNode :: ChildNode -> Maybe HTMLScriptElement
fromChildNode = unsafeReadProtoTagged "HTMLScriptElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLScriptElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLScriptElement"

fromParentNode :: ParentNode -> Maybe HTMLScriptElement
fromParentNode = unsafeReadProtoTagged "HTMLScriptElement"

fromEventTarget :: EventTarget -> Maybe HTMLScriptElement
fromEventTarget = unsafeReadProtoTagged "HTMLScriptElement"

toHTMLElement :: HTMLScriptElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLScriptElement -> Element
toElement = unsafeCoerce

toNode :: HTMLScriptElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLScriptElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLScriptElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLScriptElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLScriptElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import src :: HTMLScriptElement -> Effect String
foreign import setSrc :: String -> HTMLScriptElement -> Effect Unit

foreign import type_ :: HTMLScriptElement -> Effect String
foreign import setType :: String -> HTMLScriptElement -> Effect Unit

foreign import charset :: HTMLScriptElement -> Effect String
foreign import setCharset :: String -> HTMLScriptElement -> Effect Unit

foreign import async :: HTMLScriptElement -> Effect Boolean
foreign import setAsync :: Boolean -> HTMLScriptElement -> Effect Unit

foreign import defer :: HTMLScriptElement -> Effect Boolean
foreign import setDefer :: Boolean -> HTMLScriptElement -> Effect Unit

foreign import crossOrigin :: HTMLScriptElement -> Effect String
foreign import setCrossOrigin :: String -> HTMLScriptElement -> Effect Unit

foreign import text :: HTMLScriptElement -> Effect String
foreign import setText :: String -> HTMLScriptElement -> Effect Unit
