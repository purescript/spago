module Web.HTML.Event.DragEvent where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.HTML.Event.DataTransfer (DataTransfer)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data DragEvent :: Type

fromEvent :: Event -> Maybe DragEvent
fromEvent = unsafeReadProtoTagged "DragEvent"

toEvent :: DragEvent -> Event
toEvent = unsafeCoerce

foreign import dataTransfer :: DragEvent -> DataTransfer
