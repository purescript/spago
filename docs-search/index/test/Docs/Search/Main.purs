module Test.Docs.Search.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Test.Declarations as Declarations
import Test.IndexBuilder as IndexBuilder
import Test.ModuleIndex as ModuleIndex
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config as Config
import Test.TypeQuery as TypeQuery

testConfig :: Config.TestRunConfig
testConfig = Config.defaultConfig
  { timeout = Just (Milliseconds 5_000.0)
  }

main :: Effect Unit
main = do
  config <- Config.fromCommandLine' testConfig Config.commandLineOptionParsers
  runSpecAndExitProcess' config [ consoleReporter ] mainTest

mainTest :: Spec Unit
mainTest = do
  TypeQuery.tests
  IndexBuilder.tests
  Declarations.tests
  ModuleIndex.tests
