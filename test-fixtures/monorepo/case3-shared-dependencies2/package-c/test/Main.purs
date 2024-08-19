module Test.PACKAGE.C where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Src.PACKAGE.C as PACKAGE.C

main :: Effect Unit
main = do
  log $ "Test for " <> PACKAGE.C.packageNameValue
