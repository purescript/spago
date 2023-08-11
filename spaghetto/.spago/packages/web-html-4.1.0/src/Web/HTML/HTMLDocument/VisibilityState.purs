module Web.HTML.HTMLDocument.VisibilityState where

import Prelude

import Data.Maybe (Maybe(..))

data VisibilityState
  = Visible
  | Hidden

derive instance eqVisibilityState :: Eq VisibilityState
derive instance ordVisibilityState :: Ord VisibilityState

instance showVisibilityState :: Show VisibilityState where
  show = case _ of
    Visible -> "Visible"
    Hidden -> "Hidden"

print :: VisibilityState -> String
print = case _ of
  Visible -> "visible"
  Hidden -> "hidden"

parse :: String -> Maybe VisibilityState
parse = case _ of
  "visible" -> Just Visible
  "hidden" -> Just Hidden
  _ -> Nothing
