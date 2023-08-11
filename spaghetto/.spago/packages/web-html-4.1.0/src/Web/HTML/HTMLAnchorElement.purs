module Web.HTML.HTMLAnchorElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.DOMTokenList (DOMTokenList)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLHyperlinkElementUtils (HTMLHyperlinkElementUtils)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLAnchorElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLAnchorElement
fromHTMLElement = unsafeReadProtoTagged "HTMLAnchorElement"

fromElement :: Element -> Maybe HTMLAnchorElement
fromElement = unsafeReadProtoTagged "HTMLAnchorElement"

fromNode :: Node -> Maybe HTMLAnchorElement
fromNode = unsafeReadProtoTagged "HTMLAnchorElement"

fromChildNode :: ChildNode -> Maybe HTMLAnchorElement
fromChildNode = unsafeReadProtoTagged "HTMLAnchorElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLAnchorElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLAnchorElement"

fromParentNode :: ParentNode -> Maybe HTMLAnchorElement
fromParentNode = unsafeReadProtoTagged "HTMLAnchorElement"

fromEventTarget :: EventTarget -> Maybe HTMLAnchorElement
fromEventTarget = unsafeReadProtoTagged "HTMLAnchorElement"

toHTMLElement :: HTMLAnchorElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLAnchorElement -> Element
toElement = unsafeCoerce

toNode :: HTMLAnchorElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLAnchorElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLAnchorElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLAnchorElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLAnchorElement -> EventTarget
toEventTarget = unsafeCoerce

toHTMLHyperlinkElementUtils :: HTMLAnchorElement -> HTMLHyperlinkElementUtils
toHTMLHyperlinkElementUtils = unsafeCoerce

foreign import target :: HTMLAnchorElement -> Effect String
foreign import setTarget :: String -> HTMLAnchorElement -> Effect Unit

foreign import download :: HTMLAnchorElement -> Effect String
foreign import setDownload :: String -> HTMLAnchorElement -> Effect Unit

foreign import rel :: HTMLAnchorElement -> Effect String
foreign import setRel :: String -> HTMLAnchorElement -> Effect Unit

foreign import rev :: HTMLAnchorElement -> Effect String
foreign import setRev :: String -> HTMLAnchorElement -> Effect Unit

foreign import relList :: HTMLAnchorElement -> Effect DOMTokenList

foreign import hreflang :: HTMLAnchorElement -> Effect String
foreign import setHreflang :: String -> HTMLAnchorElement -> Effect Unit

foreign import type_ :: HTMLAnchorElement -> Effect String
foreign import setType :: String -> HTMLAnchorElement -> Effect Unit

foreign import text :: HTMLAnchorElement -> Effect String
foreign import setText :: String -> HTMLAnchorElement -> Effect Unit
