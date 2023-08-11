module Web.HTML.HTMLMediaElement.ReadyState (ReadyState(..)) where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc)
import Data.Maybe (Maybe(..))

data ReadyState
  = HaveNothing
  | HaveMetadata
  | HaveCurrentData
  | HaveFutureData
  | HaveEnoughData

derive instance eqReadyState :: Eq ReadyState
derive instance ordReadyState :: Ord ReadyState

instance boundedReadyState :: Bounded ReadyState where
  bottom = HaveNothing
  top = HaveEnoughData

instance enumReadyState :: Enum ReadyState where
  succ = defaultSucc toEnumReadyState fromEnumReadyState
  pred = defaultPred toEnumReadyState fromEnumReadyState

instance boundedEnumReadyState :: BoundedEnum ReadyState where
  cardinality = Cardinality 5
  toEnum = toEnumReadyState
  fromEnum = fromEnumReadyState

instance showReadyState :: Show ReadyState where
  show HaveNothing = "HaveNothing"
  show HaveMetadata = "HaveMetadata"
  show HaveCurrentData = "HaveCurrentData"
  show HaveFutureData = "HaveFutureData"
  show HaveEnoughData = "HaveEnoughData"

toEnumReadyState :: Int -> Maybe ReadyState
toEnumReadyState =
  case _ of
    0 -> Just HaveNothing
    1 -> Just HaveMetadata
    2 -> Just HaveCurrentData
    3 -> Just HaveFutureData
    4 -> Just HaveEnoughData
    _ -> Nothing

fromEnumReadyState :: ReadyState -> Int
fromEnumReadyState =
  case _ of
    HaveNothing -> 0
    HaveMetadata -> 1
    HaveCurrentData -> 2
    HaveFutureData -> 3
    HaveEnoughData -> 4
