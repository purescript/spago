module Web.HTML.HTMLDocument
  ( HTMLDocument
  , fromDocument
  , fromNode
  , fromParentNode
  , fromNonElementParentNode
  , fromEventTarget
  , toDocument
  , toNode
  , toParentNode
  , toNonElementParentNode
  , toEventTarget
  , documentElement
  , head
  , body
  , readyState
  , visibilityState
  , activeElement
  , currentScript
  , referrer
  , title
  , setTitle
  ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document)
import Web.DOM.Internal.Types (Node)
import Web.DOM.NonElementParentNode (NonElementParentNode)
import Web.DOM.ParentNode (ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLDocument.ReadyState (ReadyState)
import Web.HTML.HTMLDocument.ReadyState as ReadyState
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState)
import Web.HTML.HTMLDocument.VisibilityState as VisibilityState
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLHtmlElement (HTMLHtmlElement)
import Web.HTML.HTMLScriptElement (HTMLScriptElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLDocument :: Type

fromDocument :: Document -> Maybe HTMLDocument
fromDocument = unsafeReadProtoTagged "HTMLDocument"

fromNode :: Node -> Maybe HTMLDocument
fromNode = unsafeReadProtoTagged "HTMLDocument"

fromParentNode :: ParentNode -> Maybe HTMLDocument
fromParentNode = unsafeReadProtoTagged "HTMLDocument"

fromNonElementParentNode :: NonElementParentNode -> Maybe HTMLDocument
fromNonElementParentNode = unsafeReadProtoTagged "HTMLDocument"

fromEventTarget :: EventTarget -> Maybe HTMLDocument
fromEventTarget = unsafeReadProtoTagged "HTMLDocument"

toDocument :: HTMLDocument -> Document
toDocument = unsafeCoerce

toNode :: HTMLDocument -> Node
toNode = unsafeCoerce

toParentNode :: HTMLDocument -> ParentNode
toParentNode = unsafeCoerce

toNonElementParentNode :: HTMLDocument -> NonElementParentNode
toNonElementParentNode = unsafeCoerce

toEventTarget :: HTMLDocument -> EventTarget
toEventTarget = unsafeCoerce

foreign import _documentElement :: EffectFn1 HTMLDocument (Nullable HTMLHtmlElement)

documentElement :: HTMLDocument -> Effect (Maybe HTMLHtmlElement)
documentElement doc = toMaybe <$> runEffectFn1 _documentElement doc

foreign import _head :: EffectFn1 HTMLDocument (Nullable HTMLElement)

head :: HTMLDocument -> Effect (Maybe HTMLElement)
head doc = toMaybe <$> runEffectFn1 _head doc

foreign import _body :: EffectFn1 HTMLDocument (Nullable HTMLElement)

body :: HTMLDocument -> Effect (Maybe HTMLElement)
body doc = toMaybe <$> runEffectFn1 _body doc

foreign import _readyState :: EffectFn1 HTMLDocument String

readyState :: HTMLDocument -> Effect ReadyState
readyState doc = (fromMaybe ReadyState.Loading <<< ReadyState.parse) <$> (runEffectFn1 _readyState doc)

foreign import _visibilityState :: EffectFn1 HTMLDocument String

visibilityState :: HTMLDocument -> Effect VisibilityState
visibilityState doc = (fromMaybe VisibilityState.Visible <<< VisibilityState.parse) <$> (runEffectFn1 _visibilityState doc)

foreign import _activeElement :: EffectFn1 HTMLDocument (Nullable HTMLElement)

activeElement :: HTMLDocument -> Effect (Maybe HTMLElement)
activeElement doc = toMaybe <$> (runEffectFn1 _activeElement doc)

foreign import _currentScript :: EffectFn1 HTMLDocument (Nullable HTMLScriptElement)

currentScript :: HTMLDocument -> Effect (Maybe HTMLScriptElement)
currentScript doc = toMaybe <$> (runEffectFn1 _currentScript doc)

foreign import _referrer :: EffectFn1 HTMLDocument String

referrer :: HTMLDocument -> Effect String
referrer doc = runEffectFn1 _referrer doc

foreign import _title :: EffectFn1 HTMLDocument String

title :: HTMLDocument -> Effect String
title doc = runEffectFn1 _title doc

foreign import _setTitle :: EffectFn2 String HTMLDocument Unit

setTitle :: String -> HTMLDocument -> Effect Unit
setTitle newTitle doc = runEffectFn2 _setTitle newTitle doc
