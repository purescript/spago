module Spago.Psa.Printer.Json
  ( print
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Effect (Effect)
import Effect.Console as Console
import Spago.Psa.Output (Output)
import Spago.Psa.Types (encodePsaResult)

print :: Output -> Effect Unit
print output = do
  let
    result = encodePsaResult
      { warnings: _.error <$> output.warnings
      , errors: _.error <$> output.errors
      }

  Console.error (stringify result)
