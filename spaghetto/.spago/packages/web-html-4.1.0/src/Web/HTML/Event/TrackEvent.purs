module Web.HTML.Event.TrackEvent where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data TrackEvent :: Type

fromEvent :: Event -> Maybe TrackEvent
fromEvent = unsafeReadProtoTagged "TrackEvent"

toEvent :: TrackEvent -> Event
toEvent = unsafeCoerce

-- readonly attribute (VideoTrack or AudioTrack or TextTrack)? track;
