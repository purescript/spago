module Web.HTML.Event.PopStateEvent where

import Data.Maybe (Maybe)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data PopStateEvent :: Type

fromEvent :: Event -> Maybe PopStateEvent
fromEvent = unsafeReadProtoTagged "PopStateEvent"

toEvent :: PopStateEvent -> Event
toEvent = unsafeCoerce

foreign import state :: PopStateEvent -> Foreign
