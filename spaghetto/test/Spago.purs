module Test.Spago where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Test.Spago.Lock as Lock
import Test.Spec as Spec
import Test.Spec.Reporter as Spec.Reporter
import Test.Spec.Runner as Spec.Runner
import Test.Spago.Config (spec) as Config

main :: Effect Unit
main = Aff.launchAff_ $ Spec.Runner.runSpec [ Spec.Reporter.consoleReporter ] do
  Spec.describe "Spago" do
    Lock.spec
    Config.spec
