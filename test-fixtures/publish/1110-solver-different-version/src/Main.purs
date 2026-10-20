module Lib where

import Prelude

import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Console (logShow)

printNothing :: Effect Unit
printNothing = logShow $ isNothing Nothing
