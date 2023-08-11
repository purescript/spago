module Web.Event.CustomEvent where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Foreign (Foreign)
import Unsafe.Coerce as U
import Web.Event.Event (Event, EventType)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data CustomEvent :: Type

fromEvent :: Event -> Maybe CustomEvent
fromEvent = unsafeReadProtoTagged "CustomEvent"

toEvent :: CustomEvent -> Event
toEvent = U.unsafeCoerce

foreign import new :: EventType -> Effect CustomEvent

-- | Create a new `CustomEvent`, storing some data in its `detail` field,
-- | and using defaults for everything else.
new'
  :: forall a
   . EventType
  -> Maybe a
  -> Effect CustomEvent
new' ty det =
  newWithOptions ty { detail: det, bubbles: false, cancelable: false, composed: false }

-- | Create a new `CustomEvent` with all of its constructor's options exposed.
foreign import newOptionsImpl
  :: forall a
   . EventType
   -> { detail :: Nullable a , bubbles :: Boolean, cancelable :: Boolean, composed :: Boolean }
   -> Effect CustomEvent

newWithOptions
  :: forall a
   . EventType
   -> { detail :: Maybe a , bubbles :: Boolean, cancelable :: Boolean, composed :: Boolean }
   -> Effect CustomEvent
newWithOptions ty rec@{ bubbles, cancelable, composed } =
  newOptionsImpl ty { detail: toNullable rec.detail, bubbles, cancelable, composed }

foreign import detail :: CustomEvent -> Foreign
