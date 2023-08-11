module Web.HTML.HTMLMediaElement
  ( HTMLMediaElement
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
  , currentSrc
  , crossOrigin
  , setCrossOrigin
  , networkState
  , preload
  , setPreload
  , load
  , canPlayType
  , readyState
  , seeking
  , currentTime
  , setCurrentTime
  , duration
  , getStartDate
  , paused
  , defaultPlaybackRate
  , setDefaultPlaybackRate
  , playbackRate
  , setPlaybackRate
  , ended
  , autoplay
  , setAutoplay
  , loop
  , setLoop
  , play
  , pause
  , mediaGroup
  , setMediaGroup
  , controls
  , setControls
  , volume
  , setVolume
  , muted
  , setMuted
  , defaultMuted
  , setDefaultMuted
  ) where

import Prelude

import Data.Enum (toEnum)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLMediaElement.CanPlayType (CanPlayType)
import Web.HTML.HTMLMediaElement.CanPlayType as CanPlayType
import Web.HTML.HTMLMediaElement.NetworkState (NetworkState)
import Web.HTML.HTMLMediaElement.NetworkState as NetworkState
import Web.HTML.HTMLMediaElement.ReadyState (ReadyState)
import Web.HTML.HTMLMediaElement.ReadyState as ReadyState
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLMediaElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLMediaElement
fromHTMLElement = unsafeReadProtoTagged "HTMLMediaElement"

fromElement :: Element -> Maybe HTMLMediaElement
fromElement = unsafeReadProtoTagged "HTMLMediaElement"

fromNode :: Node -> Maybe HTMLMediaElement
fromNode = unsafeReadProtoTagged "HTMLMediaElement"

fromChildNode :: ChildNode -> Maybe HTMLMediaElement
fromChildNode = unsafeReadProtoTagged "HTMLMediaElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLMediaElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLMediaElement"

fromParentNode :: ParentNode -> Maybe HTMLMediaElement
fromParentNode = unsafeReadProtoTagged "HTMLMediaElement"

fromEventTarget :: EventTarget -> Maybe HTMLMediaElement
fromEventTarget = unsafeReadProtoTagged "HTMLMediaElement"

toHTMLElement :: HTMLMediaElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLMediaElement -> Element
toElement = unsafeCoerce

toNode :: HTMLMediaElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLMediaElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLMediaElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLMediaElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLMediaElement -> EventTarget
toEventTarget = unsafeCoerce

--   readonly attribute MediaError? error;

foreign import src :: HTMLMediaElement -> Effect String
foreign import setSrc :: String -> HTMLMediaElement -> Effect Unit

foreign import currentSrc :: HTMLMediaElement -> Effect String

foreign import crossOrigin :: HTMLMediaElement -> Effect String
foreign import setCrossOrigin :: String -> HTMLMediaElement -> Effect Unit

networkState :: HTMLMediaElement -> Effect NetworkState
networkState el = map (fromMaybe NetworkState.Empty <<< toEnum) $ runEffectFn1 _networkState el

foreign import _networkState :: EffectFn1 HTMLMediaElement Int

foreign import preload :: HTMLMediaElement -> Effect String
foreign import setPreload :: String -> HTMLMediaElement -> Effect Unit

--   readonly attribute TimeRanges buffered;

foreign import load :: HTMLMediaElement -> Effect Unit

canPlayType :: String -> HTMLMediaElement -> Effect CanPlayType
canPlayType ty el = map (fromMaybe CanPlayType.Unspecified <<< CanPlayType.parse) $ runEffectFn2 _canPlayType ty el

foreign import _canPlayType :: EffectFn2 String HTMLMediaElement String

readyState :: HTMLMediaElement -> Effect ReadyState
readyState el = map (fromMaybe ReadyState.HaveNothing <<< toEnum) $ runEffectFn1 _readyState el

foreign import _readyState :: EffectFn1 HTMLMediaElement Int

foreign import seeking :: HTMLMediaElement -> Effect Boolean

foreign import currentTime :: HTMLMediaElement -> Effect Number
foreign import setCurrentTime :: Number -> HTMLMediaElement -> Effect Unit

foreign import duration :: HTMLMediaElement -> Effect Number

foreign import getStartDate :: HTMLMediaElement -> Effect JSDate

foreign import paused :: HTMLMediaElement -> Effect Boolean

foreign import defaultPlaybackRate :: HTMLMediaElement -> Effect Number
foreign import setDefaultPlaybackRate :: Number -> HTMLMediaElement -> Effect Unit

foreign import playbackRate :: HTMLMediaElement -> Effect Number
foreign import setPlaybackRate :: Number -> HTMLMediaElement -> Effect Unit

--   readonly attribute TimeRanges played;
--   readonly attribute TimeRanges seekable;

foreign import ended :: HTMLMediaElement -> Effect Boolean

foreign import autoplay :: HTMLMediaElement -> Effect Boolean
foreign import setAutoplay :: Boolean -> HTMLMediaElement -> Effect Unit

foreign import loop :: HTMLMediaElement -> Effect Boolean
foreign import setLoop :: Boolean -> HTMLMediaElement -> Effect Unit

foreign import play :: HTMLMediaElement -> Effect Unit

foreign import pause :: HTMLMediaElement -> Effect Unit

foreign import mediaGroup :: HTMLMediaElement -> Effect String
foreign import setMediaGroup :: String -> HTMLMediaElement -> Effect Unit

--            attribute MediaController? controller;

foreign import controls :: HTMLMediaElement -> Effect Boolean
foreign import setControls :: Boolean -> HTMLMediaElement -> Effect Unit

foreign import volume :: HTMLMediaElement -> Effect Number
foreign import setVolume :: Number -> HTMLMediaElement -> Effect Unit

foreign import muted :: HTMLMediaElement -> Effect Boolean
foreign import setMuted :: Boolean -> HTMLMediaElement -> Effect Unit

foreign import defaultMuted :: HTMLMediaElement -> Effect Boolean
foreign import setDefaultMuted :: Boolean -> HTMLMediaElement -> Effect Unit

--   readonly attribute AudioTrackList audioTracks;
--   readonly attribute VideoTrackList videoTracks;
--   readonly attribute TextTrackList textTracks;
--   TextTrack addTextTrack(TextTrackKind kind, optional DOMString label = "", optional DOMString language = "");
