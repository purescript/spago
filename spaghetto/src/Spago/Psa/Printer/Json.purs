module Spago.Psa.Printer.Json
  ( print
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Codec.Argonaut as CA
import Effect (Effect)
import Effect.Console as Console
import Spago.Psa.Output (Output)
import Spago.Psa.Types (psaResultCodec)

print :: Output -> Effect Unit
print output = do
  let
    result = CA.encode psaResultCodec
      { warnings: _.error <$> output.warnings
      , errors: _.error <$> output.errors
      }

  Console.error (stringify result)
