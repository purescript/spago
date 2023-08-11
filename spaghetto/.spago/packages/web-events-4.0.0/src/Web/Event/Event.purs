module Web.Event.Event
  ( module Exports
  , EventType(..)
  , type_
  , target
  , currentTarget
  , eventPhase
  , stopPropagation
  , stopImmediatePropagation
  , bubbles
  , cancelable
  , preventDefault
  , defaultPrevented
  , timeStamp
  ) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Enum (toEnum)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Web.Event.EventPhase (EventPhase)
import Web.Event.Internal.Types (Event) as Exports
import Web.Event.Internal.Types (Event, EventTarget)

-- | The type of strings used for event types.
newtype EventType = EventType String

derive instance newtypeEventType :: Newtype EventType _
derive newtype instance eqEventType :: Eq EventType
derive newtype instance ordEventType :: Ord EventType

-- | The event type.
foreign import type_ :: Event -> EventType

-- | The element that was the source of the event.
target :: Event -> Maybe EventTarget
target = toMaybe <<< _target

foreign import _target :: Event -> Nullable EventTarget

-- | The element that the event listener was added to.
currentTarget :: Event -> Maybe EventTarget
currentTarget = toMaybe <<< _currentTarget

foreign import _currentTarget :: Event -> Nullable EventTarget

-- | Indicates which phase of the event flow that is currently being processed
-- | for the event.
eventPhase :: Partial => Event -> EventPhase
eventPhase = fromJust <<< toEnum <<< eventPhaseIndex

-- | The integer value for the current event phase.
foreign import eventPhaseIndex :: Event -> Int

-- | Prevents the event from bubbling up to futher event listeners. Other event
-- | listeners on the current target will still fire.
foreign import stopPropagation :: Event -> Effect Unit

-- | Prevents all other listeners for the event from being called. This includes
-- | event listeners added to the current target after the current listener.
foreign import stopImmediatePropagation :: Event -> Effect Unit

-- | Indicates whether the event will bubble up through the DOM or not.
foreign import bubbles :: Event -> Boolean

-- | Indicates whether the event can be cancelled.
foreign import cancelable :: Event -> Boolean

-- | Cancels the event if it can be cancelled.
foreign import preventDefault :: Event -> Effect Unit

-- | Indicates whether `preventDefault` was called on the event.
foreign import defaultPrevented :: Event -> Effect Boolean

-- | The time in milliseconds between 01/01/1970 and when the event was
-- | dispatched.
foreign import timeStamp :: Event -> Instant
