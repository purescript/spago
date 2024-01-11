module Test.Docs.Search.Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Declarations as Declarations
import Test.IndexBuilder as IndexBuilder
import Test.ModuleIndex as ModuleIndex
import Test.ModuleParser as ModuleParser
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (Config, defaultConfig, runSpecT)
import Test.TypeQuery as TypeQuery

testConfig :: Config
testConfig = defaultConfig
  { slow = Milliseconds 2_000.0
  , timeout = Just (Milliseconds 5_000.0)
  }

main :: Effect Unit
main = launchAff_ $ void $ un Identity $ runSpecT testConfig [ consoleReporter ] mainTest

mainTest :: Spec Unit
mainTest = do
  ModuleParser.tests
  TypeQuery.tests
  IndexBuilder.tests
  Declarations.tests
  ModuleIndex.tests
