module Test.Spago.Unit where

import Prelude

import Test.Spago.Unit.CheckInjectivity as CheckInjectivity
import Test.Spago.Unit.FindFlags as FindFlags
import Test.Spago.Unit.Git as Git
import Test.Spago.Unit.Init as Init
import Test.Spago.Unit.NodeVersion as NodeVersion
import Test.Spago.Unit.Path as Path
import Test.Spago.Unit.Printer as Printer
import Test.Spago.Unit.Run as Run
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.describe "unit" do
  FindFlags.spec
  CheckInjectivity.spec
  Init.spec
  Printer.spec
  Git.spec
  Path.spec
  NodeVersion.spec
  Run.spec
