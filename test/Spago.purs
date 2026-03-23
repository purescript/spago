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

main :: Effect Unit
main =
  runSpecAndExitProcess'
    { defaultConfig: Cfg.defaultConfig { timeout = Just (Milliseconds 120_000.0) }
    , parseCLIOptions: true
    }
    [ Spec.Reporter.consoleReporter ]
    do
      Spec.describe "spago" do
        -- A few of the test suites are hard to parallelise.
        -- E.g. the build one runs so many instances of the compiler that they easily
        -- segfault, which makes the tests unreliable.
        -- Or e.g. some of these remove the global cache, which would definitely mess up
        -- other tests running in parallel to it.
        -- So we run these problematic suites first, sequentially, before running the
        -- rest of the suites with parallelism.
        Build.lockfileSpec
        Build.spec
        Publish.spec
        Transfer.spec
        Glob.spec

        Spec.parallel $ Cli.spec
        Spec.parallel $ Init.spec
        Spec.parallel $ Sources.spec
        Spec.parallel $ Install.spec
        Spec.parallel $ Uninstall.spec
        Spec.parallel $ Ls.spec
        Spec.parallel $ Repl.spec
        Spec.parallel $ Run.spec
        Spec.parallel $ Test.spec
        Spec.parallel $ Bundle.spec
        Spec.parallel $ Registry.spec
        Spec.parallel $ Docs.spec
        Spec.parallel $ Upgrade.spec
        Spec.parallel $ Graph.spec
        Spec.parallel $ Lock.spec
        Spec.parallel $ Unit.spec
        Spec.parallel $ Errors.spec
        Spec.parallel $ Config.spec
        Spec.parallel $ Install.forceResetSpec
