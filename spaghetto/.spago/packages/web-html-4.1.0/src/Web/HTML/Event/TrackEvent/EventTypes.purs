module Web.HTML.Event.TrackEvent.EventTypes where

import Web.Event.Event (EventType(..))

addtrack :: EventType
addtrack = EventType "addtrack"

removetrack :: EventType
removetrack = EventType "removetrack"
