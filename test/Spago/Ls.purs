module Test.Spago.Ls where

import Test.Prelude

import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "ls" do

    Spec.it "direct dependencies" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "ls", "deps" ] >>= shouldBeSuccessOutput (fixture "list-dependencies.txt")

    Spec.it "direct dependencies in JSON, and requires selecting a package when many are present" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaa" ] >>= shouldBeSuccess
      _ <- makeSubpackage testCwd { name: "subpackage", moduleName: "Subpackage" }
      spago [ "install", "-p", "aaa", "subpackage" ] >>= shouldBeSuccess
      spago [ "ls", "deps", "-p", "aaa", "--json" ] >>= shouldBeSuccessOutput (fixture "list-dependencies.json")

    Spec.it "package set" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      spago [ "ls", "packages" ] >>= shouldBeSuccessOutput (fixture "list-packages.txt")

    Spec.it "package set in JSON" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaa", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      _ <- makeSubpackage testCwd { name: "subpackage", moduleName: "Subpackage" }
      spago [ "install", "-p", "aaa", "subpackage" ] >>= shouldBeSuccess
      spago [ "ls", "packages", "--json" ] >>= shouldBeSuccessOutput (fixture "list-packages.json")

    Spec.it "can't list package set if we are solving with the Registry" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa", "--use-solver" ] >>= shouldBeSuccess
      spago [ "ls", "packages" ] >>= shouldBeFailureErr (fixture "list-packages-registry.txt")
