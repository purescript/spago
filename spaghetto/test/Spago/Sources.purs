module Test.Spago.Sources where

import Test.Prelude

import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "sources" do

    Spec.it "prints both dependencies and project sources" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "sources" ] >>= shouldBeSuccessOutput (fixture "sources-output.txt")
