module Web.HTML.HTMLMetaElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLMetaElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLMetaElement
fromHTMLElement = unsafeReadProtoTagged "HTMLMetaElement"

fromElement :: Element -> Maybe HTMLMetaElement
fromElement = unsafeReadProtoTagged "HTMLMetaElement"

fromNode :: Node -> Maybe HTMLMetaElement
fromNode = unsafeReadProtoTagged "HTMLMetaElement"

fromChildNode :: ChildNode -> Maybe HTMLMetaElement
fromChildNode = unsafeReadProtoTagged "HTMLMetaElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLMetaElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLMetaElement"

fromParentNode :: ParentNode -> Maybe HTMLMetaElement
fromParentNode = unsafeReadProtoTagged "HTMLMetaElement"

fromEventTarget :: EventTarget -> Maybe HTMLMetaElement
fromEventTarget = unsafeReadProtoTagged "HTMLMetaElement"

toHTMLElement :: HTMLMetaElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLMetaElement -> Element
toElement = unsafeCoerce

toNode :: HTMLMetaElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLMetaElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLMetaElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLMetaElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLMetaElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import name :: HTMLMetaElement -> Effect String
foreign import setName :: String -> HTMLMetaElement -> Effect Unit

foreign import httpEquiv :: HTMLMetaElement -> Effect String
foreign import setHttpEquiv :: String -> HTMLMetaElement -> Effect Unit

foreign import content :: HTMLMetaElement -> Effect String
foreign import setContent :: String -> HTMLMetaElement -> Effect Unit
