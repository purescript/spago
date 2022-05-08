module Test.Main where

import Prelude

import Effect (Effect)
import Test.Declarations as Declarations
import Test.IndexBuilder as IndexBuilder
import Test.ModuleIndex as ModuleIndex
import Test.TypeQuery as TypeQuery
import Test.TypeJson as TypeJson
import Test.UI as UI
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [consoleReporter] mainTest
  UI.main

mainTest :: Spec Unit
mainTest = do
  TypeQuery.tests
  TypeJson.tests
  IndexBuilder.tests
  Declarations.tests
  ModuleIndex.tests
