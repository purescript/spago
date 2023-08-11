module Routing.Duplex.Types where

import Data.Tuple (Tuple)

type RouteParams = Array (Tuple String String)

type RouteState =
  { segments :: Array String
  , params :: RouteParams
  , hash :: String
  }

emptyRouteState :: RouteState
emptyRouteState =
  { segments: []
  , params: []
  , hash: ""
  }
