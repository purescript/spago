module Test.PACKAGE.A where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Src.PACKAGE.A as PACKAGE.A
import Src.PACKAGE.C as PACKAGE.C
import Src.PACKAGE.B as PACKAGE.B

main :: Effect Unit
main = do
  log $ "Test for " <> PACKAGE.A.packageNameValue <> PACKAGE.C.packageNameValue <> PACKAGE.B.packageNameValue