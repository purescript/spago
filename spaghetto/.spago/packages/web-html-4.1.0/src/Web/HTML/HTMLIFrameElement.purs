module Web.HTML.HTMLIFrameElement
  ( HTMLIFrameElement
  , fromHTMLElement
  , fromElement
  , fromNode
  , fromChildNode
  , fromNonDocumentTypeChildNode
  , fromParentNode
  , fromEventTarget
  , toHTMLElement
  , toElement
  , toNode
  , toChildNode
  , toNonDocumentTypeChildNode
  , toParentNode
  , toEventTarget
  , src
  , setSrc
  , srcdoc
  , setSrcdoc
  , name
  , setName
  , width
  , setWidth
  , height
  , setHeight
  , contentDocument
  , contentWindow
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.Document (Document)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.Window (Window)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLIFrameElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLIFrameElement
fromHTMLElement = unsafeReadProtoTagged "HTMLIFrameElement"

fromElement :: Element -> Maybe HTMLIFrameElement
fromElement = unsafeReadProtoTagged "HTMLIFrameElement"

fromNode :: Node -> Maybe HTMLIFrameElement
fromNode = unsafeReadProtoTagged "HTMLIFrameElement"

fromChildNode :: ChildNode -> Maybe HTMLIFrameElement
fromChildNode = unsafeReadProtoTagged "HTMLIFrameElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLIFrameElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLIFrameElement"

fromParentNode :: ParentNode -> Maybe HTMLIFrameElement
fromParentNode = unsafeReadProtoTagged "HTMLIFrameElement"

fromEventTarget :: EventTarget -> Maybe HTMLIFrameElement
fromEventTarget = unsafeReadProtoTagged "HTMLIFrameElement"

toHTMLElement :: HTMLIFrameElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLIFrameElement -> Element
toElement = unsafeCoerce

toNode :: HTMLIFrameElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLIFrameElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLIFrameElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLIFrameElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLIFrameElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import src :: HTMLIFrameElement -> Effect String
foreign import setSrc :: String -> HTMLIFrameElement -> Effect Unit

foreign import srcdoc :: HTMLIFrameElement -> Effect String
foreign import setSrcdoc :: String -> HTMLIFrameElement -> Effect Unit

foreign import name :: HTMLIFrameElement -> Effect String
foreign import setName :: String -> HTMLIFrameElement -> Effect Unit

--   [PutForwards=value] readonly attribute DOMSettableTokenList sandbox;

foreign import width :: HTMLIFrameElement -> Effect String
foreign import setWidth :: String -> HTMLIFrameElement -> Effect Unit

foreign import height :: HTMLIFrameElement -> Effect String
foreign import setHeight :: String -> HTMLIFrameElement -> Effect Unit

foreign import _contentDocument :: HTMLIFrameElement -> Effect (Nullable Document)
foreign import _contentWindow :: HTMLIFrameElement -> Effect (Nullable Window)

contentDocument :: HTMLIFrameElement -> Effect (Maybe Document)
contentDocument = map toMaybe <<< _contentDocument

contentWindow :: HTMLIFrameElement -> Effect (Maybe Window)
contentWindow = map toMaybe <<< _contentWindow
