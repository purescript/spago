module Subpackage.B.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Subpackage.Shared.Lib as Shared

main :: Effect Unit
main = do
  log $ packageName <> " is not " <> Shared.packageName

packageName :: String
packageName = "packageB"

