module Test.PACKAGE.A where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Src.PACKAGE.A as PACKAGE.A

main :: Effect Unit
main = do
  log $ "Test for " <> PACKAGE.A.packageNameValue <> "no deps"