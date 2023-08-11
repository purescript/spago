module Web.XHR.ProgressEvent where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data ProgressEvent :: Type

fromEvent :: Event -> Maybe ProgressEvent
fromEvent = unsafeReadProtoTagged "ProgressEvent"

toEvent :: ProgressEvent -> Event
toEvent = unsafeCoerce

foreign import lengthComputable :: ProgressEvent -> Boolean

foreign import loaded :: ProgressEvent -> Number

foreign import total :: ProgressEvent -> Number
