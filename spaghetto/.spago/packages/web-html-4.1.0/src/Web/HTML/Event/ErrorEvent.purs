module Web.HTML.Event.ErrorEvent where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data ErrorEvent :: Type

fromEvent :: Event -> Maybe ErrorEvent
fromEvent = unsafeReadProtoTagged "ErrorEvent"

toEvent :: ErrorEvent -> Event
toEvent = unsafeCoerce

foreign import message :: ErrorEvent -> String

foreign import fileName :: ErrorEvent -> String

foreign import lineNo :: ErrorEvent -> Int

foreign import colNo :: ErrorEvent -> Int
