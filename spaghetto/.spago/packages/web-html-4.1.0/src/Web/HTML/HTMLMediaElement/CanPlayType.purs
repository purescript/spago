module Web.HTML.HTMLMediaElement.CanPlayType where

import Prelude

import Data.Maybe (Maybe(..))

data CanPlayType
  = Unspecified
  | Maybe
  | Probably

derive instance eqCanPlayType :: Eq CanPlayType
derive instance ordCanPlayType :: Ord CanPlayType

instance showCanPlayType :: Show CanPlayType where
  show = case _ of
    Unspecified -> "Unspecified"
    Maybe -> "Maybe"
    Probably -> "Probably"

parse :: String -> Maybe CanPlayType
parse = case _ of
  "" -> Just Unspecified
  "maybe" -> Just Maybe
  "probably" -> Just Probably
  _ -> Nothing

print :: CanPlayType -> String
print = case _ of
  Unspecified -> ""
  Maybe -> "maybe"
  Probably -> "probably"
