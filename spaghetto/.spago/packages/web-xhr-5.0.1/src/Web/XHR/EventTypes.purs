module Web.XHR.EventTypes where

import Web.Event.Event (EventType(..))

loadstart :: EventType
loadstart = EventType "loadstart"

progress :: EventType
progress = EventType "progress"

abort :: EventType
abort = EventType "abort"

error :: EventType
error = EventType "error"

load :: EventType
load = EventType "load"

timeout :: EventType
timeout = EventType "timeout"

loadend :: EventType
loadend = EventType "loadend"
