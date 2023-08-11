module Web.HTML.HTMLMediaElement.NetworkState (NetworkState(..)) where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc)
import Data.Maybe (Maybe(..))

data NetworkState
  = Empty
  | Idle
  | Loading
  | NoSource

derive instance eqNetworkState :: Eq NetworkState
derive instance ordNetworkState :: Ord NetworkState

instance boundedNetworkState :: Bounded NetworkState where
  bottom = Empty
  top = NoSource

instance enumNetworkState :: Enum NetworkState where
  succ = defaultSucc toEnumNetworkState fromEnumNetworkState
  pred = defaultPred toEnumNetworkState fromEnumNetworkState

instance boundedEnumNetworkState :: BoundedEnum NetworkState where
  cardinality = Cardinality 4
  toEnum = toEnumNetworkState
  fromEnum = fromEnumNetworkState

instance showNetworkState :: Show NetworkState where
  show Empty = "Empty"
  show Idle = "Idle"
  show Loading = "Loading"
  show NoSource = "NoSource"

toEnumNetworkState :: Int -> Maybe NetworkState
toEnumNetworkState =
  case _ of
    0 -> Just Empty
    1 -> Just Idle
    2 -> Just Loading
    3 -> Just NoSource
    _ -> Nothing

fromEnumNetworkState :: NetworkState -> Int
fromEnumNetworkState =
  case _ of
    Empty -> 0
    Idle -> 1
    Loading -> 2
    NoSource -> 3
