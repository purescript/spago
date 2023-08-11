module Web.HTML.Event.EventTypes where

import Web.Event.Event (EventType(..))

abort :: EventType
abort = EventType "abort"

afterprint :: EventType
afterprint = EventType "afterprint"

afterscriptexecute :: EventType
afterscriptexecute = EventType "afterscriptexecute"

beforeprint :: EventType
beforeprint = EventType "beforeprint"

beforescriptexecute :: EventType
beforescriptexecute = EventType "beforescriptexecute"

blur :: EventType
blur = EventType "blur"

cancel :: EventType
cancel = EventType "cancel"

change :: EventType
change = EventType "change"

click :: EventType
click = EventType "click"

close :: EventType
close = EventType "close"

copy :: EventType
copy = EventType "copy"

cut :: EventType
cut = EventType "cut"

domcontentloaded :: EventType
domcontentloaded = EventType "DOMContentLoaded"

error :: EventType
error = EventType "error"

focus :: EventType
focus = EventType "focus"

input :: EventType
input = EventType "input"

invalid :: EventType
invalid = EventType "invalid"

languagechange :: EventType
languagechange = EventType "languagechange"

load :: EventType
load = EventType "load"

loadend :: EventType
loadend = EventType "loadend"

loadstart :: EventType
loadstart = EventType "loadstart"

message :: EventType
message = EventType "message"

offline :: EventType
offline = EventType "offline"

online :: EventType
online = EventType "online"

paste :: EventType
paste = EventType "paste"

progress :: EventType
progress = EventType "progress"

readystatechange :: EventType
readystatechange = EventType "readystatechange"

reset :: EventType
reset = EventType "reset"

select :: EventType
select = EventType "select"

storage :: EventType
storage = EventType "storage"

submit :: EventType
submit = EventType "submit"

toggle :: EventType
toggle = EventType "toggle"

unload :: EventType
unload = EventType "unload"
