module Web.File.FileReader.ReadyState where

import Prelude
import Data.Enum (Cardinality(..), class BoundedEnum, defaultPred, defaultSucc, class Enum)
import Data.Maybe (Maybe(..))

data ReadyState
  = EMPTY
  | LOADING
  | DONE

derive instance eqReadyState :: Eq ReadyState
derive instance ordReadyState :: Ord ReadyState

instance boundedReadyState :: Bounded ReadyState where
  bottom = EMPTY
  top = DONE

instance enumReadyState :: Enum ReadyState where
  succ = defaultSucc toEnumReadyState fromEnumReadyState
  pred = defaultPred toEnumReadyState fromEnumReadyState

instance boundedEnumReadyState :: BoundedEnum ReadyState where
  cardinality = Cardinality 3
  toEnum = toEnumReadyState
  fromEnum = fromEnumReadyState

instance showReadyState :: Show ReadyState where
  show EMPTY = "EMPTY"
  show LOADING = "LOADING"
  show DONE = "DONE"

toEnumReadyState :: Int -> Maybe ReadyState
toEnumReadyState =
  case _ of
    0 -> Just EMPTY
    1 -> Just LOADING
    2 -> Just DONE
    _ -> Nothing

fromEnumReadyState :: ReadyState -> Int
fromEnumReadyState =
  case _ of
    EMPTY -> 0
    LOADING -> 1
    DONE -> 2
