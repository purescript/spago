module Web.HTML.HTMLSourceElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLSourceElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLSourceElement
fromHTMLElement = unsafeReadProtoTagged "HTMLSourceElement"

fromElement :: Element -> Maybe HTMLSourceElement
fromElement = unsafeReadProtoTagged "HTMLSourceElement"

fromNode :: Node -> Maybe HTMLSourceElement
fromNode = unsafeReadProtoTagged "HTMLSourceElement"

fromChildNode :: ChildNode -> Maybe HTMLSourceElement
fromChildNode = unsafeReadProtoTagged "HTMLSourceElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLSourceElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLSourceElement"

fromParentNode :: ParentNode -> Maybe HTMLSourceElement
fromParentNode = unsafeReadProtoTagged "HTMLSourceElement"

fromEventTarget :: EventTarget -> Maybe HTMLSourceElement
fromEventTarget = unsafeReadProtoTagged "HTMLSourceElement"

toHTMLElement :: HTMLSourceElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLSourceElement -> Element
toElement = unsafeCoerce

toNode :: HTMLSourceElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLSourceElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLSourceElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLSourceElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLSourceElement -> EventTarget
toEventTarget = unsafeCoerce

-- [NamedConstructor=Audio(optional DOMString src)]

foreign import src :: HTMLSourceElement -> Effect String
foreign import setSrc :: String -> HTMLSourceElement -> Effect Unit

foreign import type_ :: HTMLSourceElement -> Effect String
foreign import setType :: String -> HTMLSourceElement -> Effect Unit

foreign import media :: HTMLSourceElement -> Effect String
foreign import setMedia :: String -> HTMLSourceElement -> Effect Unit
