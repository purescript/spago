module Web.HTML.HTMLTrackElement.ReadyState where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc)
import Data.Maybe (Maybe(..))

data ReadyState
  = None
  | Loading
  | Loaded
  | Error

derive instance eqReadyState :: Eq ReadyState
derive instance ordReadyState :: Ord ReadyState

instance boundedReadyState :: Bounded ReadyState where
  bottom = None
  top = Error

instance enumReadyState :: Enum ReadyState where
  succ = defaultSucc toEnumReadyState fromEnumReadyState
  pred = defaultPred toEnumReadyState fromEnumReadyState

instance boundedEnumReadyState :: BoundedEnum ReadyState where
  cardinality = Cardinality 4
  toEnum = toEnumReadyState
  fromEnum = fromEnumReadyState

instance showReadyState :: Show ReadyState where
  show None = "None"
  show Loading = "Loading"
  show Loaded = "Loaded"
  show Error = "Error"

toEnumReadyState :: Int -> Maybe ReadyState
toEnumReadyState =
  case _ of
    0 -> Just None
    1 -> Just Loading
    2 -> Just Loaded
    3 -> Just Error
    _ -> Nothing

fromEnumReadyState :: ReadyState -> Int
fromEnumReadyState =
  case _ of
    None -> 0
    Loading -> 1
    Loaded -> 2
    Error -> 3
