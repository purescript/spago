module Test.Spago where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Test.Spago.Build as Build
import Test.Spago.Bundle as Bundle
import Test.Spago.Docs as Docs
import Test.Spago.Graph as Graph
import Test.Spago.Init as Init
import Test.Spago.Install as Install
import Test.Spago.Lock as Lock
import Test.Spago.Ls as Ls
import Test.Spago.Publish as Publish
import Test.Spago.Registry as Registry
import Test.Spago.Run as Run
import Test.Spago.Sources as Sources
import Test.Spago.Test as Test
import Test.Spago.Unit as Unit
import Test.Spago.Upgrade as Upgrade
import Test.Spec as Spec
import Test.Spec.Reporter as Spec.Reporter
import Test.Spec.Runner as Spec.Runner

testConfig :: Spec.Runner.Config
testConfig =
  { slow: Milliseconds 10_000.0
  , timeout: Just (Milliseconds 90_000.0)
  , exit: true
  }

main :: Effect Unit
main = Aff.launchAff_ $ void $ un Identity $ Spec.Runner.runSpecT testConfig [ Spec.Reporter.consoleReporter ] do
  Spec.describe "spago" do
    -- TODO: script
    Init.spec
    Sources.spec
    Install.spec
    Ls.spec
    Build.spec
    Run.spec
    Test.spec
    Bundle.spec
    Registry.spec
    Docs.spec
    Upgrade.spec
    Publish.spec
    Graph.spec
    Spec.describe "miscellaneous" do
      Lock.spec
      Unit.spec

