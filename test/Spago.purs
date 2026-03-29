module Test.Spago where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.AVar as Effect.AVar
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

main :: Effect Unit
main = do
  -- Per-command locks: one mutex per compiler-triggering command.
  -- Two builds can't run concurrently, but a build and a test can.
  cmdLocks <- Map.fromFoldable <$> traverse (\cmd -> Tuple cmd <$> Effect.AVar.new unit)
    [ "build", "test", "run", "bundle", "install", "fetch" ]
  runSpecAndExitProcess'
    { defaultConfig: Cfg.defaultConfig { timeout = Just (Milliseconds 600_000.0) }
    , parseCLIOptions: true
    }
    [ Spec.Reporter.consoleReporter ]
    do
      Spec.describe "spago" do
        -- A few of the test suites are hard to parallelise.
        -- E.g. some of these remove the global cache, which would definitely mess up
        -- other tests running in parallel to it.
        -- So we run these problematic suites first, sequentially, before running the
        -- rest of the suites with parallelism.
        Build.lockfileSpec cmdLocks
        -- Publish/Transfer assume a warm registry cache from earlier tests,
        -- so they must stay sequential.
        Publish.spec
        Transfer.spec

        Build.spec cmdLocks
        Cli.spec
        Init.spec
        Sources.spec
        Install.spec cmdLocks
        Uninstall.spec cmdLocks
        Ls.spec cmdLocks
        Repl.spec
        Run.spec cmdLocks
        Test.spec cmdLocks
        Bundle.spec cmdLocks
        Registry.spec
        Docs.spec
        Upgrade.spec cmdLocks
        Graph.spec
        Lock.spec cmdLocks
        Unit.spec
        Errors.spec cmdLocks
        Config.spec
        Glob.spec
        Install.forceResetSpec
