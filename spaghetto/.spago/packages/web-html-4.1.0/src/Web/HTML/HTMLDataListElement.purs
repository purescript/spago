module Web.HTML.HTMLDataListElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.HTMLCollection (HTMLCollection)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLDataListElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLDataListElement
fromHTMLElement = unsafeReadProtoTagged "HTMLDataListElement"

fromElement :: Element -> Maybe HTMLDataListElement
fromElement = unsafeReadProtoTagged "HTMLDataListElement"

fromNode :: Node -> Maybe HTMLDataListElement
fromNode = unsafeReadProtoTagged "HTMLDataListElement"

fromChildNode :: ChildNode -> Maybe HTMLDataListElement
fromChildNode = unsafeReadProtoTagged "HTMLDataListElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLDataListElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLDataListElement"

fromParentNode :: ParentNode -> Maybe HTMLDataListElement
fromParentNode = unsafeReadProtoTagged "HTMLDataListElement"

fromEventTarget :: EventTarget -> Maybe HTMLDataListElement
fromEventTarget = unsafeReadProtoTagged "HTMLDataListElement"

toHTMLElement :: HTMLDataListElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLDataListElement -> Element
toElement = unsafeCoerce

toNode :: HTMLDataListElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLDataListElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLDataListElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLDataListElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLDataListElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import options :: HTMLDataListElement -> Effect HTMLCollection
