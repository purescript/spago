module Subpackage.A.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Subpackage.B.Main as B
import Subpackage.C.Lib as C

main :: Effect Unit
main = do
  log $ packageName <> " is not " <> C.packageName <> " or " <> B.packageName

packageName :: String
packageName = "packageA"

