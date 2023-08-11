module Web.HTML.HTMLImageElement.CORSMode
  ( CORSMode(..)
  , parse
  , print
  ) where

import Data.Maybe (Maybe(..))
import Prelude (class Eq, class Ord, class Show)

data CORSMode
  = Anonymous
  | UseCredentials

derive instance eqCORSMode :: Eq CORSMode
derive instance ordCORSMode :: Ord CORSMode

instance showDecodingHint :: Show CORSMode where
  show = case _ of
    Anonymous -> "Anonymous"
    UseCredentials -> "UseCredentials"

parse :: String -> Maybe CORSMode
parse = case _ of
  "" -> Just Anonymous
  "anonymous" -> Just Anonymous
  "use-credentials" -> Just UseCredentials
  _ -> Nothing

print :: CORSMode -> String
print = case _ of
  Anonymous -> "anonymous"
  UseCredentials -> "use-credentials"
