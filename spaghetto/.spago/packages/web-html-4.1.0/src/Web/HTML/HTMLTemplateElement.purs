module Web.HTML.HTMLTemplateElement where

import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.DocumentFragment (DocumentFragment)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLTemplateElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLTemplateElement
fromHTMLElement = unsafeReadProtoTagged "HTMLTemplateElement"

fromElement :: Element -> Maybe HTMLTemplateElement
fromElement = unsafeReadProtoTagged "HTMLTemplateElement"

fromNode :: Node -> Maybe HTMLTemplateElement
fromNode = unsafeReadProtoTagged "HTMLTemplateElement"

fromChildNode :: ChildNode -> Maybe HTMLTemplateElement
fromChildNode = unsafeReadProtoTagged "HTMLTemplateElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLTemplateElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLTemplateElement"

fromParentNode :: ParentNode -> Maybe HTMLTemplateElement
fromParentNode = unsafeReadProtoTagged "HTMLTemplateElement"

fromEventTarget :: EventTarget -> Maybe HTMLTemplateElement
fromEventTarget = unsafeReadProtoTagged "HTMLTemplateElement"

toHTMLElement :: HTMLTemplateElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLTemplateElement -> Element
toElement = unsafeCoerce

toNode :: HTMLTemplateElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLTemplateElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLTemplateElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLTemplateElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLTemplateElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import content :: HTMLTemplateElement -> Effect DocumentFragment
