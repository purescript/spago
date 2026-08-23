module Test.Spago.Script where

import Test.Prelude

import Spago.Path as Path
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: CommandLocks -> Spec Unit
spec locks = Spec.parallel $ Spec.around (withBuildLock locks) do
  Spec.describe "script" do
    Spec.it "runs a standalone source file from the caller's directory" \{ spago, fixture, testCwd } -> do
      let source = fixture "spago-script-make-file.purs"
      spago [ "script", "-d", "node-fs", Path.toRaw source ] >>= shouldBeSuccess
      checkFixture (testCwd </> "spago-script-result.txt") (fixture "spago-script-result.txt")
