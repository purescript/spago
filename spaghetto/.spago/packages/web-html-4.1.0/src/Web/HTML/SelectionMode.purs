module Web.HTML.SelectionMode where

import Prelude

import Data.Maybe (Maybe(..))

data SelectionMode
  = Preserve
  | Select
  | Start
  | End

derive instance eqSelectionMode :: Eq SelectionMode
derive instance ordSelectionMode :: Ord SelectionMode

instance showSelectionMode :: Show SelectionMode where
  show = case _ of
    Preserve -> "Preserve"
    Select -> "Select"
    Start -> "Start"
    End -> "End"

parse :: String -> Maybe SelectionMode
parse = case _ of
  "preserve" -> Just Preserve
  "select" -> Just Select
  "start" -> Just Start
  "end" -> Just End
  _ -> Nothing

print :: SelectionMode -> String
print = case _ of
  Preserve -> "preserve"
  Select -> "select"
  Start -> "start"
  End -> "end"
