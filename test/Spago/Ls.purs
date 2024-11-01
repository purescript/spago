module Test.Spago.Ls where

import Test.Prelude

import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
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
      makeSubpackage testCwd
      spago [ "install", "-p", "aaa", "aaa2" ] >>= shouldBeSuccess
      spago [ "ls", "deps", "-p", "aaa", "--json" ] >>= shouldBeSuccessOutput (fixture "list-dependencies.json")

    Spec.it "package set" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      spago [ "ls", "packages" ] >>= shouldBeSuccessOutput (fixture "list-packages.txt")

    Spec.it "package set in JSON" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaa", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      makeSubpackage testCwd
      spago [ "install", "-p", "aaa", "aaa2" ] >>= shouldBeSuccess
      spago [ "ls", "packages", "--json" ] >>= shouldBeSuccessOutput (fixture "list-packages.json")

    Spec.it "can't list package set if we are solving with the Registry" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa", "--use-solver" ] >>= shouldBeSuccess
      spago [ "ls", "packages" ] >>= shouldBeFailureErr (fixture "list-packages-registry.txt")

makeSubpackage :: RootPath -> Aff Unit
makeSubpackage root = do
  let subpackage = root </> "subpackage"
  FS.mkdirp (subpackage </> "src")
  FS.mkdirp (subpackage </> "test")
  FS.writeTextFile (subpackage </> "src" </> "Main.purs") (Init.srcMainTemplate "Subpackage.Main")
  FS.writeTextFile (subpackage </> "test" </> "Main.purs") (Init.testMainTemplate "Subpackage.Test.Main")
  FS.writeYamlFile Config.configCodec (subpackage </> "spago.yaml")
    ( Init.defaultConfig
        { name: mkPackageName "aaa2"
        , withWorkspace: Nothing
        , testModuleName: "Subpackage.Test.Main"
        }
    )
