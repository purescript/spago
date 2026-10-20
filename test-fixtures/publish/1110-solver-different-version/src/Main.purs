module Lib where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)

printNothing :: Effect Unit
printNothing = logShow unit
