module Web.Event.EventPhase where

import Prelude
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), defaultSucc, defaultPred)
import Data.Maybe (Maybe(..))

data EventPhase
  = None
  | Capturing
  | AtTarget
  | Bubbling

derive instance eqEventPhase :: Eq EventPhase
derive instance ordEventPhase :: Ord EventPhase

instance boundedEventPhase :: Bounded EventPhase where
  bottom = None
  top = Bubbling

instance enumEventPhase :: Enum EventPhase where
  succ = defaultSucc toEnumEventPhase fromEnumEventPhase
  pred = defaultPred toEnumEventPhase fromEnumEventPhase

instance boundedEnumEventPhase :: BoundedEnum EventPhase where
  cardinality = Cardinality 4
  toEnum = toEnumEventPhase
  fromEnum = fromEnumEventPhase

toEnumEventPhase :: Int -> Maybe EventPhase
toEnumEventPhase =
  case _ of
    0 -> Just None
    1 -> Just Capturing
    2 -> Just AtTarget
    3 -> Just Bubbling
    _ -> Nothing

fromEnumEventPhase :: EventPhase -> Int
fromEnumEventPhase =
  case _ of
    None -> 0
    Capturing -> 1
    AtTarget -> 2
    Bubbling -> 3
