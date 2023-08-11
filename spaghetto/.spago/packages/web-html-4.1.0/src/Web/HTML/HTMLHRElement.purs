module Web.HTML.HTMLHRElement where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLHRElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLHRElement
fromHTMLElement = unsafeReadProtoTagged "HTMLHRElement"

fromElement :: Element -> Maybe HTMLHRElement
fromElement = unsafeReadProtoTagged "HTMLHRElement"

fromNode :: Node -> Maybe HTMLHRElement
fromNode = unsafeReadProtoTagged "HTMLHRElement"

fromChildNode :: ChildNode -> Maybe HTMLHRElement
fromChildNode = unsafeReadProtoTagged "HTMLHRElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLHRElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLHRElement"

fromParentNode :: ParentNode -> Maybe HTMLHRElement
fromParentNode = unsafeReadProtoTagged "HTMLHRElement"

fromEventTarget :: EventTarget -> Maybe HTMLHRElement
fromEventTarget = unsafeReadProtoTagged "HTMLHRElement"

toHTMLElement :: HTMLHRElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLHRElement -> Element
toElement = unsafeCoerce

toNode :: HTMLHRElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLHRElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLHRElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLHRElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLHRElement -> EventTarget
toEventTarget = unsafeCoerce
