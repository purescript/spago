module Web.HTML.HTMLParamElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLParamElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLParamElement
fromHTMLElement = unsafeReadProtoTagged "HTMLParamElement"

fromElement :: Element -> Maybe HTMLParamElement
fromElement = unsafeReadProtoTagged "HTMLParamElement"

fromNode :: Node -> Maybe HTMLParamElement
fromNode = unsafeReadProtoTagged "HTMLParamElement"

fromChildNode :: ChildNode -> Maybe HTMLParamElement
fromChildNode = unsafeReadProtoTagged "HTMLParamElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLParamElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLParamElement"

fromParentNode :: ParentNode -> Maybe HTMLParamElement
fromParentNode = unsafeReadProtoTagged "HTMLParamElement"

fromEventTarget :: EventTarget -> Maybe HTMLParamElement
fromEventTarget = unsafeReadProtoTagged "HTMLParamElement"

toHTMLElement :: HTMLParamElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLParamElement -> Element
toElement = unsafeCoerce

toNode :: HTMLParamElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLParamElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLParamElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLParamElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLParamElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import name :: HTMLParamElement -> Effect String
foreign import setName :: String -> HTMLParamElement -> Effect Unit

foreign import value :: HTMLParamElement -> Effect String
foreign import setValue :: String -> HTMLParamElement -> Effect Unit
