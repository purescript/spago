module Web.HTML.HTMLImageElement.Laziness
  ( Laziness(..)
  , parse
  , print
  ) where

import Data.Maybe (Maybe(..))
import Prelude (class Eq, class Ord, class Show)

data Laziness
  = Eager
  | Lazy

derive instance eqDecodingHint :: Eq Laziness
derive instance ordDecodingHint :: Ord Laziness

instance showDecodingHint :: Show Laziness where
  show = case _ of
    Eager -> "Eager"
    Lazy -> "Lazy"

parse :: String -> Maybe Laziness
parse = case _ of
  "" -> Just Eager
  "eager" -> Just Eager
  "lazy" -> Just Lazy
  _ -> Nothing

print :: Laziness -> String
print = case _ of
  Eager -> "eager"
  Lazy -> "lazy"
