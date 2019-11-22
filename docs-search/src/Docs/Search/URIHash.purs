module Docs.Search.URIHash
       ( getInput
       , setInput
       , removeHash
       )
where

import Prelude

import Data.Maybe (Maybe(Just), fromMaybe)
import Data.String.CodeUnits as String
import Effect (Effect)
import Global (decodeURIComponent, encodeURIComponent)
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window

foreign import removeHash :: Effect Unit

setInput :: String -> Effect Unit
setInput "" = removeHash
setInput input = do
  window <- HTML.window
  location <- Window.location window
  let hash = "search:" <> fromMaybe "" (encodeURIComponent input)
  Location.setHash hash location

getInput :: Effect String
getInput = do
  window <- HTML.window
  location <- Window.location window
  hash <- Location.hash location
  pure $
    if String.slice 0 8 hash == Just "#search:"
    then fromMaybe "" $
         decodeURIComponent $
         String.drop 8 hash
    else ""
