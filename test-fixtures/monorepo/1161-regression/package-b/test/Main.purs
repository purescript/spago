module Test.PACKAGE.B where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Src.PACKAGE.B as PACKAGE.B
import Data.Either

main :: Effect Unit
main = do
  log $ "Test for " <> PACKAGE.B.packageNameValue <> EITHER.packageNameValue