module Test.PACKAGE.B where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Src.PACKAGE.B as PACKAGE.B
import Src.PACKAGE.C as PACKAGE.C

main :: Effect Unit
main = do
  log $ "Test for " <> PACKAGE.B.packageNameValue <> PACKAGE.C.packageNameValue