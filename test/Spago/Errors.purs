module Test.Spago.Errors where

import Test.Prelude

import Data.Array as Array
import Data.String as String
import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "errors" do

    Spec.it "fails with a spago.yml" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa" ] >>= shouldBeSuccess
      FS.moveSync { src: "spago.yaml", dst: "spago.yml" }
      spago [ "build" ] >>= shouldBeFailureErr (fixture "spago-yml-check-stderr.txt")

    Spec.it "fails for package names that are too long" \{ spago, fixture } -> do
      let name = String.joinWith "" $ Array.replicate 256 "a"
      spago [ "init", "--name", name ] >>= shouldBeFailureErr (fixture "package-name-too-long-stderr.txt")
