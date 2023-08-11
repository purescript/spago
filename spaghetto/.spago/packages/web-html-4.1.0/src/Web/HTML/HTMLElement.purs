module Web.HTML.HTMLElement
  ( HTMLElement
  , fromElement
  , fromNode
  , fromChildNode
  , fromNonDocumentTypeChildNode
  , fromParentNode
  , fromEventTarget
  , toElement
  , toNode
  , toChildNode
  , toNonDocumentTypeChildNode
  , toParentNode
  , toEventTarget
  , title
  , setTitle
  , lang
  , setLang
  , dir
  , setDir
  , hidden
  , setHidden
  , tabIndex
  , setTabIndex
  , draggable
  , setDraggable
  , contentEditable
  , setContentEditable
  , isContentEditable
  , spellcheck
  , setSpellcheck
  , click
  , focus
  , blur
  , offsetParent
  , offsetTop
  , offsetLeft
  , offsetWidth
  , offsetHeight
  ) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode)
import Web.DOM.Element (Element)
import Web.DOM.Internal.Types (Node)
import Web.DOM.NonDocumentTypeChildNode (NonDocumentTypeChildNode)
import Web.DOM.ParentNode (ParentNode)
import Web.Event.EventTarget (EventTarget)

foreign import data HTMLElement :: Type

foreign import _read :: forall a. Fn3 (forall x. Maybe x) (forall x. x -> Maybe x) a (Maybe HTMLElement)

fromElement :: Element -> Maybe HTMLElement
fromElement x = runFn3 _read Nothing Just x

fromNode :: Node -> Maybe HTMLElement
fromNode x = runFn3 _read Nothing Just x

fromChildNode :: ChildNode -> Maybe HTMLElement
fromChildNode x = runFn3 _read Nothing Just x

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLElement
fromNonDocumentTypeChildNode x = runFn3 _read Nothing Just x

fromParentNode :: ParentNode -> Maybe HTMLElement
fromParentNode x = runFn3 _read Nothing Just x

fromEventTarget :: EventTarget -> Maybe HTMLElement
fromEventTarget x = runFn3 _read Nothing Just x

toElement :: HTMLElement -> Element
toElement = unsafeCoerce

toNode :: HTMLElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import title :: HTMLElement -> Effect String
foreign import setTitle :: String -> HTMLElement -> Effect Unit

foreign import lang :: HTMLElement -> Effect String
foreign import setLang :: String -> HTMLElement -> Effect Unit

foreign import dir :: HTMLElement -> Effect String
foreign import setDir :: String -> HTMLElement -> Effect Unit

foreign import hidden :: HTMLElement -> Effect Boolean
foreign import setHidden :: Boolean -> HTMLElement -> Effect Unit

foreign import tabIndex :: HTMLElement -> Effect Int
foreign import setTabIndex :: Int -> HTMLElement -> Effect Unit

foreign import draggable :: HTMLElement -> Effect Boolean
foreign import setDraggable :: Boolean -> HTMLElement -> Effect Unit

foreign import contentEditable :: HTMLElement -> Effect String
foreign import setContentEditable :: String -> HTMLElement -> Effect Unit
foreign import isContentEditable :: HTMLElement -> Effect Boolean

foreign import spellcheck :: HTMLElement -> Effect Boolean
foreign import setSpellcheck :: Boolean -> HTMLElement -> Effect Unit

foreign import click :: HTMLElement -> Effect Unit
foreign import focus :: HTMLElement -> Effect Unit
foreign import blur :: HTMLElement -> Effect Unit

foreign import _offsetParent :: HTMLElement -> Effect (Nullable Element)

offsetParent :: HTMLElement -> Effect (Maybe Element)
offsetParent = map toMaybe <<< _offsetParent

foreign import offsetTop :: HTMLElement -> Effect Number
foreign import offsetLeft :: HTMLElement -> Effect Number
foreign import offsetWidth :: HTMLElement -> Effect Number
foreign import offsetHeight :: HTMLElement -> Effect Number
