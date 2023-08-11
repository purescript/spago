module Web.HTML.HTMLDocument.ReadyState where

import Prelude

import Data.Maybe (Maybe(..))

data ReadyState
  = Loading
  | Interactive
  | Complete

derive instance eqReadyState :: Eq ReadyState
derive instance ordReadyState :: Ord ReadyState

instance showReadyState :: Show ReadyState where
  show = case _ of
    Loading -> "Loading"
    Interactive -> "Interactive"
    Complete -> "Complete"

print :: ReadyState -> String
print = case _ of
  Loading -> "loading"
  Interactive -> "interactive"
  Complete -> "complete"

parse :: String -> Maybe ReadyState
parse = case _ of
  "loading" -> Just Loading
  "interactive" -> Just Interactive
  "complete" -> Just Complete
  _ -> Nothing
