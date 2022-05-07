module Test.Main where

import Prelude

import Effect (Effect)
import Test.Declarations as Declarations
import Test.IndexBuilder as IndexBuilder
import Test.ModuleIndex as ModuleIndex
import Test.TypeQuery as TypeQuery
import Test.TypeJson as TypeJson
import Test.UI as UI
import Test.Unit (TestSuite)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest mainTest
  UI.main

mainTest :: TestSuite
mainTest = do
  TypeQuery.tests
  TypeJson.tests
  IndexBuilder.tests
  Declarations.tests
  ModuleIndex.tests
