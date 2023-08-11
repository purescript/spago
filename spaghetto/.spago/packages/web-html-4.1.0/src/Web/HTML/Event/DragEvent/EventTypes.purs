module Web.HTML.Event.DragEvent.EventTypes where

import Web.Event.Event (EventType(..))

dragstart :: EventType
dragstart = EventType "dragstart"

drag :: EventType
drag = EventType "drag"

dragenter :: EventType
dragenter = EventType "dragenter"

dragexit :: EventType
dragexit = EventType "dragexit"

dragleave :: EventType
dragleave = EventType "dragleave"

dragover :: EventType
dragover = EventType "dragover"

drop :: EventType
drop = EventType "drop"

dragend :: EventType
dragend = EventType "dragend"
