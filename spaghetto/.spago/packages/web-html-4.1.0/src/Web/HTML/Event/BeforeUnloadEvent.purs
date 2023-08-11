module Web.HTML.Event.BeforeUnloadEvent where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data BeforeUnloadEvent :: Type

fromEvent :: Event -> Maybe BeforeUnloadEvent
fromEvent = unsafeReadProtoTagged "BeforeUnloadEvent"

toEvent :: BeforeUnloadEvent -> Event
toEvent = unsafeCoerce

foreign import returnValue :: BeforeUnloadEvent -> Effect String

foreign import setReturnValue :: String -> BeforeUnloadEvent -> Effect Unit
