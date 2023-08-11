module Web.HTML.Event.PageTransitionEvent.EventTypes where

import Web.Event.Event (EventType(..))

pagehide :: EventType
pagehide = EventType "pagehide"

pageshow :: EventType
pageshow = EventType "pageshow"
