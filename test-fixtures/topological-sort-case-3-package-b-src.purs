module Subpackage.B.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Subpackage.C.Lib as C

main :: Effect Unit
main = do
  log $ packageName <> " is not " <> C.packageName

packageName :: String
packageName = "packageB"

