module Test.Spago.Unit where

import Prelude

import Test.Spago.Unit.CheckInjectivity as CheckInjectivity
import Test.Spago.Unit.FindFlags as FindFlags
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.describe "unit" do
  FindFlags.spec
  CheckInjectivity.spec
