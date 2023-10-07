module Test.Spago.Test where

import Test.Prelude

import Data.Array as Array
import Data.String as String
import Node.Path as Path
import Registry.Version as Version
import Spago.Command.Init (DefaultConfigOptions(..))
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
            { name: mkPackageName "subpackage"
            , withWorkspace: Nothing
            , testModuleName: "Subpackage.Test.Main"
            }
        )
      spago [ "test", "-p", "subpackage" ] >>= shouldBeSuccess

    Spec.it "runs tests from a sub-package in the current working directory, not the sub-package's directory" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp "subpackage/src"
      FS.mkdirp "subpackage/test"
      FS.writeTextFile "subpackage/src/Main.purs" (Init.srcMainTemplate "Subpackage.Main")

      -- We write a file into the current working directory. 
      -- The subpackage test will read the given file without changing its directory
      -- and log its content as its output.
      let textFilePath = "foo.txt"
      let fileContent = "foo"
      FS.writeTextFile textFilePath fileContent
      FS.copyFile
        { src: fixture "spago-subpackage-test-cwd.purs"
        , dst: "subpackage/test/Main.purs"
        }
      FS.writeYamlFile Config.configCodec "subpackage/spago.yaml"
        ( ( Init.defaultConfig
              { name: mkPackageName "subpackage"
              , withWorkspace: Nothing
              , testModuleName: "Subpackage.Test.Main"
              }
          ) # plusDependencies [ "aff", "node-buffer", "node-fs" ]
        )
      spago [ "test", "-p", "subpackage" ] >>= checkOutputsStr { stdoutStr: Just fileContent, stderrStr: Nothing, result: isRight }

    Spec.it "fails when running tests from a sub-package, where the module does not exist" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp "subpackage/src"
      FS.mkdirp "subpackage/test"
      FS.writeTextFile "subpackage/src/Main.purs" (Init.srcMainTemplate "Subpackage.Main")
      FS.writeTextFile "subpackage/test/Main.purs" (Init.testMainTemplate "Subpackage.Test.Main2")
      FS.writeYamlFile Config.configCodec "subpackage/spago.yaml"
        ( Init.defaultConfig
            { name: mkPackageName "subpackage"
            , withWorkspace: Nothing
            , testModuleName: "Subpackage.Test.Main"
            }
        )
      spago [ "test", "-p", "subpackage" ] >>= shouldBeFailure

    Spec.it "can use a custom output folder" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "test", "--output", "myOutput" ] >>= shouldBeSuccess
      FS.exists "myOutput" `Assert.shouldReturn` true

    Spec.it "'strict: true' on package src does not cause test code containing warnings to fail to build" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      -- add --strict
      FS.writeYamlFile Config.configCodec "spago.yaml" $ Init.defaultConfig' $ PackageAndWorkspace
        { name: mkPackageName "package-a"
        , dependencies: [ "prelude", "effect", "console" ]
        , test: Just { moduleMain: "Test.Main" }
        , build: Just { strict: Just true, censorProjectCodes: Nothing }
        }
        { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1" }

      -- add version where test file has warning
      FS.writeTextFile (Path.concat [ "test", "Test", "Main.purs" ]) $ Array.intercalate "\n"
        [ "module Test.Main where"
        , ""
        , "import Prelude"
        , "import Effect (Effect)"
        , "import Effect.Class.Console (log)"
        , "main :: Effect Unit"
        , "main = bar 1"
        , ""
        , "bar :: Int -> Effect Unit"
        , "bar unusedName = do"
        , "  log \"🍕\""
        , "  log \"You should add some tests.\""
        , ""
        ]
      let
        hasUnusedNameWarningError stdErr = do
          let exp = "[1/1 UnusedName] test/Test/Main.purs:10:5"
          unless (String.contains (String.Pattern exp) stdErr) do
            Assert.fail $ "STDERR did not contain text:\n" <> exp <> "\n\nStderr was:\n" <> stdErr
      spago [ "test" ] >>= check { stdout: mempty, stderr: hasUnusedNameWarningError, result: isRight }

