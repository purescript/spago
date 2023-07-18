module Test.Spago where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
-- import Test.Spago.Build as Build
-- import Test.Spago.Bundle as Bundle
import Test.Spago.Init as Init
-- import Test.Spago.Install as Install
import Test.Spago.Lock as Lock
-- import Test.Spago.Ls as Ls
-- import Test.Spago.Run as Run
import Test.Spago.Sources as Sources
-- import Test.Spago.Test as Test
import Test.Spec as Spec
import Test.Spec.Reporter as Spec.Reporter
import Test.Spec.Runner as Spec.Runner

main :: Effect Unit
main = Aff.launchAff_ $ Spec.Runner.runSpec [ Spec.Reporter.consoleReporter ] do
  Spec.describe "spago" do
    Lock.spec
    Init.spec
    -- Install.spec
    -- Build.spec
    -- Test.spec
    -- Upgrade set?
    -- Run.spec
    -- Script?
    -- Bundle.spec
    -- Ls.spec
    Sources.spec
