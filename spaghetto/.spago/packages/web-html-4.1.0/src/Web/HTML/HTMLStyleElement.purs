module Web.HTML.HTMLStyleElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLStyleElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLStyleElement
fromHTMLElement = unsafeReadProtoTagged "HTMLStyleElement"

fromElement :: Element -> Maybe HTMLStyleElement
fromElement = unsafeReadProtoTagged "HTMLStyleElement"

fromNode :: Node -> Maybe HTMLStyleElement
fromNode = unsafeReadProtoTagged "HTMLStyleElement"

fromChildNode :: ChildNode -> Maybe HTMLStyleElement
fromChildNode = unsafeReadProtoTagged "HTMLStyleElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLStyleElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLStyleElement"

fromParentNode :: ParentNode -> Maybe HTMLStyleElement
fromParentNode = unsafeReadProtoTagged "HTMLStyleElement"

fromEventTarget :: EventTarget -> Maybe HTMLStyleElement
fromEventTarget = unsafeReadProtoTagged "HTMLStyleElement"

toHTMLElement :: HTMLStyleElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLStyleElement -> Element
toElement = unsafeCoerce

toNode :: HTMLStyleElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLStyleElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLStyleElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLStyleElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLStyleElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import media :: HTMLStyleElement -> Effect String
foreign import setMedia :: String -> HTMLStyleElement -> Effect Unit

foreign import type_ :: HTMLStyleElement -> Effect String
foreign import setType :: String -> HTMLStyleElement -> Effect Unit
