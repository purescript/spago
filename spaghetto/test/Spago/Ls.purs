module Test.Spago.Ls where

import Test.Prelude

import Registry.PackageName as PackageName
import Registry.Version as Version
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

    Spec.it "direct dependencies in JSON, and requires selecting a package when many are present" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa" ] >>= shouldBeSuccess
      makeSubpackage
      spago [ "install", "-p", "aaa", "aaa2" ] >>= shouldBeSuccess
      spago [ "ls", "deps", "-p", "aaa", "--json" ] >>= shouldBeSuccessOutput (fixture "list-dependencies.json")

    Spec.it "package set" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa" ] >>= shouldBeSuccess
      spago [ "ls", "packages" ] >>= shouldBeSuccessOutput (fixture "list-packages.txt")

    Spec.it "package set in JSON" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa" ] >>= shouldBeSuccess
      makeSubpackage
      spago [ "install", "-p", "aaa", "aaa2" ] >>= shouldBeSuccess
      spago [ "ls", "packages", "--json" ] >>= shouldBeSuccessOutput (fixture "list-packages.json")

    Spec.it "can't list package set if we are solving with the Registry" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          (unsafeFromRight (PackageName.parse "aaa"))
          (Just $ unsafeFromRight $ Version.parse "0.0.1")
          "Test.Main"
      FS.writeYamlFile Config.configCodec "spago.yaml"
        (conf { workspace = conf.workspace # map (_ { package_set = Nothing }) })
      spago [ "ls", "packages" ] >>= shouldBeFailureErr (fixture "list-packages-registry.txt")

makeSubpackage :: Aff Unit
makeSubpackage = do
  FS.mkdirp "subpackage/src"
  FS.mkdirp "subpackage/test"
  FS.writeTextFile "subpackage/src/Main.purs" (Init.srcMainTemplate "Subpackage.Main")
  FS.writeTextFile "subpackage/test/Main.purs" (Init.testMainTemplate "Subpackage.Test.Main")
  FS.writeYamlFile Config.configCodec "subpackage/spago.yaml"
    ( Init.defaultConfig
        (unsafeFromRight (PackageName.parse "aaa2"))
        Nothing
        "Subpackage.Test.Main"
    )
