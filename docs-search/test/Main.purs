module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Declarations as Declarations
import Test.IndexBuilder as IndexBuilder
import Test.ModuleIndex as ModuleIndex
import Test.ModuleParser as ModuleParser
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TypeJson as TypeJson
import Test.TypeQuery as TypeQuery

main :: Effect Unit
main = do
  launchAff_ $ runSpec [consoleReporter] mainTest
  -- UI.main

mainTest :: Spec Unit
mainTest = do
  ModuleParser.tests
  TypeQuery.tests
  TypeJson.tests
  IndexBuilder.tests
  Declarations.tests
  ModuleIndex.tests
