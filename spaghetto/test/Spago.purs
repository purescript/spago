module Test.Spago where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Test.Spago.Init as Init
import Test.Spago.Install as Install
import Test.Spago.Lock as Lock
import Test.Spago.Sources as Sources
import Test.Spago.Test as Test
import Test.Spec as Spec
import Test.Spec.Reporter as Spec.Reporter
import Test.Spec.Runner as Spec.Runner

testConfig :: Spec.Runner.Config
testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 10000.0), exit: true }

main :: Effect Unit
main = Aff.launchAff_ $ void $ un Identity $ Spec.Runner.runSpecT testConfig [ Spec.Reporter.consoleReporter ] do
  Spec.describe "spago" do
    -- TODO:
    -- Build.spec
    -- Upgrade set?
    -- Run.spec
    -- Script?
    -- Bundle.spec
    -- Ls.spec

    Lock.spec
    Init.spec
    Install.spec
    Sources.spec
    Test.spec

