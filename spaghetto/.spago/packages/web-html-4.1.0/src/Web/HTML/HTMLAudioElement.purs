module Web.HTML.HTMLAudioElement
  ( HTMLAudioElement
  , fromHTMLMediaElement
  , fromHTMLElement
  , fromElement
  , fromNode
  , fromChildNode
  , fromNonDocumentTypeChildNode
  , fromParentNode
  , fromEventTarget
  , toHTMLMediaElement
  , toHTMLElement
  , toElement
  , toNode
  , toChildNode
  , toNonDocumentTypeChildNode
  , toParentNode
  , toEventTarget
  , create
  , create'
  ) where

import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLMediaElement (HTMLMediaElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLAudioElement :: Type

fromHTMLMediaElement :: HTMLMediaElement -> Maybe HTMLAudioElement
fromHTMLMediaElement = unsafeReadProtoTagged "HTMLAudioElement"

fromHTMLElement :: HTMLElement -> Maybe HTMLAudioElement
fromHTMLElement = unsafeReadProtoTagged "HTMLAudioElement"

fromElement :: Element -> Maybe HTMLAudioElement
fromElement = unsafeReadProtoTagged "HTMLAudioElement"

fromNode :: Node -> Maybe HTMLAudioElement
fromNode = unsafeReadProtoTagged "HTMLAudioElement"

fromChildNode :: ChildNode -> Maybe HTMLAudioElement
fromChildNode = unsafeReadProtoTagged "HTMLAudioElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLAudioElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLAudioElement"

fromParentNode :: ParentNode -> Maybe HTMLAudioElement
fromParentNode = unsafeReadProtoTagged "HTMLAudioElement"

fromEventTarget :: EventTarget -> Maybe HTMLAudioElement
fromEventTarget = unsafeReadProtoTagged "HTMLAudioElement"

toHTMLMediaElement :: HTMLAudioElement -> HTMLMediaElement
toHTMLMediaElement = unsafeCoerce

toHTMLElement :: HTMLAudioElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLAudioElement -> Element
toElement = unsafeCoerce

toNode :: HTMLAudioElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLAudioElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLAudioElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLAudioElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLAudioElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import create :: Effect HTMLAudioElement
foreign import createWithURL :: String -> Effect HTMLAudioElement

create' :: String -> Effect HTMLAudioElement
create' = createWithURL
