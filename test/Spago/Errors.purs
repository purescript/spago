module Test.Spago.Errors where

import Test.Prelude

import Data.Array as Array
import Data.Foldable (traverse_)
import Data.String as String
import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "errors" do

    Spec.it "fails with a spago.yml" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaa" ] >>= shouldBeSuccess
      FS.moveSync { src: testCwd </> "spago.yaml", dst: testCwd </> "spago.yml" }
      spago [ "build" ] >>= shouldBeFailureErr (fixture "spago-yml-check-stderr.txt")

    Spec.it "fails for package names that are too long" \{ spago, fixture } -> do
      let name = String.joinWith "" $ Array.replicate 256 "a"
      spago [ "init", "--name", name ] >>= shouldBeFailureErr (fixture "package-name-too-long-stderr.txt")

    Spec.it "prints suggested package names when package is not found" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "root" ] >>= shouldBeSuccess

      ["finder", "binder", "founder"] # traverse_ \name -> do
        FS.mkdirp $ testCwd </> name
        FS.writeTextFile (testCwd </> name </> "spago.yaml") $
          "{ package: { name: \"" <> name <> "\", dependencies: [] } }"

      spago [ "build", "-p", "inder" ] >>= shouldBeFailureErr (fixture "package-typo-suggestions/1.txt")
      spago [ "build", "-p", "flounder" ] >>= shouldBeFailureErr (fixture "package-typo-suggestions/2.txt")
      spago [ "build", "-p", "totally-bogus" ] >>= shouldBeFailureErr (fixture "package-typo-suggestions/3.txt")
