module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main =
  logShow $ isNothing Nothing
