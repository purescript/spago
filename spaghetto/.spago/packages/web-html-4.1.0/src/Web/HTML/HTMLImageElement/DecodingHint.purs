module Web.HTML.HTMLImageElement.DecodingHint
  ( DecodingHint(..)
  , parse
  , print
  ) where

import Data.Maybe (Maybe(..))
import Prelude (class Eq, class Ord, class Show)

data DecodingHint
  = Sync
  | Async
  | Auto

derive instance eqDecodingHint :: Eq DecodingHint
derive instance ordDecodingHint :: Ord DecodingHint

instance showDecodingHint :: Show DecodingHint where
  show = case _ of
    Sync -> "Sync"
    Async -> "Async"
    Auto -> "Auto"

parse :: String -> Maybe DecodingHint
parse = case _ of
  "" -> Just Auto
  "sync" -> Just Sync
  "async" -> Just Async
  "auto" -> Just Auto
  _ -> Nothing

print :: DecodingHint -> String
print = case _ of
  Sync -> "sync"
  Async -> "async"
  Auto -> "auto"
