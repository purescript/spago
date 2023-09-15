module Test.Spago.Test where

import Test.Prelude

import Registry.PackageName as PackageName
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "test" do

    Spec.it "tests successfully" \{ spago, fixture } -> do
      spago [ "init", "--name", "7368613235362d6a336156536c675a7033334e7659556c6d38" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "test" ] >>= shouldBeSuccessOutputWithErr (fixture "test-output-stdout.txt") (fixture "test-output-stderr.txt")

    Spec.it "fails nicely when the test module is not found" \{ spago, fixture } -> do
      spago [ "init", "--name", "7368613235362d6a336156536c675a7033334e7659556c6d38" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      FS.moveSync { dst: "test2", src: "test" }
      spago [ "test" ] >>= shouldBeFailureErr (fixture "test-missing-module.txt")

    Spec.it "runs tests from a sub-package" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp "subpackage/src"
      FS.mkdirp "subpackage/test"
      FS.writeTextFile "subpackage/src/Main.purs" (Init.srcMainTemplate "Subpackage.Main")
      FS.writeTextFile "subpackage/test/Main.purs" (Init.testMainTemplate "Subpackage.Test.Main")
      FS.writeYamlFile Config.configCodec "subpackage/spago.yaml"
        ( Init.defaultConfig
            (mkPackageName "subpackage")
            Nothing
            "Subpackage.Test.Main"
        )
      spago [ "test", "-p", "subpackage" ] >>= shouldBeSuccess

    Spec.it "runs tests from a sub-package in the current working directory, not the sub-package's directory" \{ spago, testCwd, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp "subpackage/src"
      FS.mkdirp "subpackage/test"
      FS.writeTextFile "subpackage/src/Main.purs" (Init.srcMainTemplate "Subpackage.Main")
      FS.copyFile
        { src: fixture "spago-subpackage-test-cwd.purs"
        , dst: "subpackage/test/Main.purs"
        }
      FS.writeYamlFile Config.configCodec "subpackage/spago.yaml"
        ( ( Init.defaultConfig
              (mkPackageName "subpackage")
              Nothing
              "Subpackage.Test.Main"
          ) # plusDependencies [ "node-process" ]
        )
      spago [ "test", "-p", "subpackage" ] >>= checkResultAndOutputsStr (Just testCwd) Nothing isRight

    Spec.it "fails when running tests from a sub-package, where the module does not exist" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp "subpackage/src"
      FS.mkdirp "subpackage/test"
      FS.writeTextFile "subpackage/src/Main.purs" (Init.srcMainTemplate "Subpackage.Main")
      FS.writeTextFile "subpackage/test/Main.purs" (Init.testMainTemplate "Subpackage.Test.Main2")
      FS.writeYamlFile Config.configCodec "subpackage/spago.yaml"
        ( Init.defaultConfig
            (mkPackageName "subpackage")
            Nothing
            "Subpackage.Test.Main"
        )
      spago [ "test", "-p", "subpackage" ] >>= shouldBeFailure

    Spec.it "can use a custom output folder" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "test", "--output", "myOutput" ] >>= shouldBeSuccess
      FS.exists "myOutput" `Assert.shouldReturn` true
