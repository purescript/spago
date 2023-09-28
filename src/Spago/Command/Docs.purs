module Spago.Command.Docs where

import Prelude
import Effect (Effect)
import Docs.Search.Main (main)

doc :: Effect Unit
doc = main
