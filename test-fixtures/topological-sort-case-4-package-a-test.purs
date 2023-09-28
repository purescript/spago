module Subpackage.SameName.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Subpackage.SameName.Main as Package

main :: Effect Unit
main = do
  log $ "Test for " <> Package.packageName

