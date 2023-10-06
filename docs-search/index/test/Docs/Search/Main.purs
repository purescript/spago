module Test.Docs.Search.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Declarations as Declarations
import Test.IndexBuilder as IndexBuilder
import Test.ModuleIndex as ModuleIndex
import Test.ModuleParser as ModuleParser
import Test.TypeQuery as TypeQuery
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] mainTest

mainTest :: Spec Unit
mainTest = do
  ModuleParser.tests
  TypeQuery.tests
  IndexBuilder.tests
  Declarations.tests
  ModuleIndex.tests
