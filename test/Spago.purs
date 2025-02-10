module Test.Spago where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Test.Spago.Build as Build
import Test.Spago.Bundle as Bundle
import Test.Spago.Cli as Cli
import Test.Spago.Config as Config
import Test.Spago.Docs as Docs
import Test.Spago.Errors as Errors
import Test.Spago.Glob as Glob
import Test.Spago.Graph as Graph
import Test.Spago.Init as Init
import Test.Spago.Install as Install
import Test.Spago.Lock as Lock
import Test.Spago.Ls as Ls
import Test.Spago.Publish as Publish
import Test.Spago.Registry as Registry
import Test.Spago.Repl as Repl
import Test.Spago.Run as Run
import Test.Spago.Sources as Sources
import Test.Spago.Test as Test
import Test.Spago.Transfer as Transfer
import Test.Spago.Uninstall as Uninstall
import Test.Spago.Unit as Unit
import Test.Spago.Upgrade as Upgrade
import Test.Spec as Spec
import Test.Spec.Reporter as Spec.Reporter
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config as Cfg

testConfig :: Cfg.TestRunConfig
testConfig = Cfg.defaultConfig
  { timeout = Just (Milliseconds 120_000.0)
  }

main :: Effect Unit
main = do
  config <- Cfg.fromCommandLine' testConfig Cfg.commandLineOptionParsers
  runSpecAndExitProcess' config [ Spec.Reporter.consoleReporter ] do
    Spec.describe "spago" do
      -- TODO: script
      Cli.spec
      Init.spec
      Sources.spec
      Install.spec
      Uninstall.spec
      Ls.spec
      Build.spec
      Repl.spec
      Run.spec
      Test.spec
      Bundle.spec
      Registry.spec
      Docs.spec
      Upgrade.spec
      Publish.spec
      Transfer.spec
      Graph.spec
      Spec.describe "miscellaneous" do
        Lock.spec
        Unit.spec
        Glob.spec
        Errors.spec
        Config.spec
