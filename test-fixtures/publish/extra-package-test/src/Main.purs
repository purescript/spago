module Lib where

import Prelude

import Effect (Effect)
import Effect.Console (log)

printFoo :: Effect Unit
printFoo = log "foo"
