module Web.HTML.Event.HashChangeEvent where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HashChangeEvent :: Type

fromEvent :: Event -> Maybe HashChangeEvent
fromEvent = unsafeReadProtoTagged "HashChangeEvent"

toEvent :: HashChangeEvent -> Event
toEvent = unsafeCoerce

foreign import oldURL :: HashChangeEvent -> String

foreign import newURL :: HashChangeEvent -> String
