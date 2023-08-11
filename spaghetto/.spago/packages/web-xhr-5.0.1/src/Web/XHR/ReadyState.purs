module Web.XHR.ReadyState where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc)
import Data.Maybe (Maybe(..))

data ReadyState
  = Unsent
  | Opened
  | HeadersReceived
  | Loading
  | Done

derive instance eqReadyState :: Eq ReadyState
derive instance ordReadyState :: Ord ReadyState

instance boundedReadyState :: Bounded ReadyState where
  bottom = Unsent
  top = Done

instance enumReadyState :: Enum ReadyState where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance boundedEnumReadyState :: BoundedEnum ReadyState where
  cardinality = Cardinality 5
  toEnum = toEnum
  fromEnum = fromEnum

toEnum :: Int -> Maybe ReadyState
toEnum = case _ of
  0 -> Just Unsent
  1 -> Just Opened
  2 -> Just HeadersReceived
  3 -> Just Loading
  4 -> Just Done
  _ -> Nothing

fromEnum :: ReadyState -> Int
fromEnum = case _ of
  Unsent -> 0
  Opened -> 1
  HeadersReceived -> 2
  Loading -> 3
  Done -> 4
