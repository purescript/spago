module Web.Event.EventTarget
  ( module Exports
  , EventListener
  , eventListener
  , addEventListener
  , addEventListenerWithOptions
  , removeEventListener
  , dispatchEvent
  ) where

import Prelude

import Effect (Effect)
import Web.Event.Event (EventType)
import Web.Event.Internal.Types (Event, EventTarget)
import Web.Event.Internal.Types (EventTarget) as Exports

-- | A boxed function that can be used as an event listener. This is necessary
-- | due to the underlying implementation of Effect functions.
foreign import data EventListener :: Type

-- | Creates an EventListener from a normal PureScript Effect function.
-- |
-- | This function itself is effectful as otherwise it would break referential
-- | transparency - `eventListener f /= eventListener f`. This is worth noting
-- | as you can only remove the exact event listener value that was added for
-- | an `EventTarget`.
foreign import eventListener
  :: forall a
   . (Event -> Effect a)
  -> Effect EventListener

-- | Adds a listener to an event target.
-- | - `capture` - whether the listener is added to the "capture" phase
-- | - `once` - if true, indicates listener should be invokved at most once
-- |            before being automatically removed.
-- | - `passive` - indicates the callback function will never call `preventDefault`
foreign import addEventListenerWithOptions
  :: EventType
  -> EventListener
  -> { capture :: Boolean
     , once :: Boolean
     , passive :: Boolean
     }
  -> EventTarget
  -> Effect Unit

-- | Adds a listener to an event target. The boolean argument indicates whether
-- | the listener should be added for the "capture" phase.
foreign import addEventListener
  :: EventType
  -> EventListener
  -> Boolean
  -> EventTarget
  -> Effect Unit

-- | Removes a listener to an event target. The boolean argument indicates
-- | whether the listener should be removed for the "capture" phase.
foreign import removeEventListener
  :: EventType
  -> EventListener
  -> Boolean
  -> EventTarget
  -> Effect Unit

-- | Dispatches an event from an event target.
foreign import dispatchEvent
  :: Event
  -> EventTarget
  -> Effect Boolean
