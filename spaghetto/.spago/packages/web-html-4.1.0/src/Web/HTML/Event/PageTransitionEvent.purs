module Web.HTML.Event.PageTransitionEvent where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data PageTransitionEvent :: Type

fromEvent :: Event -> Maybe PageTransitionEvent
fromEvent = unsafeReadProtoTagged "PageTransitionEvent"

toEvent :: PageTransitionEvent -> Event
toEvent = unsafeCoerce

foreign import persisted :: PageTransitionEvent -> Boolean
