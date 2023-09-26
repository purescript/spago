module Subpackage.A.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Subpackage.A.Main as Package
import Subpackage.C.Lib as C

main :: Effect Unit
main = do
  log $ "Test for " <> Package.packageName <> ", not " <> C.packageName

