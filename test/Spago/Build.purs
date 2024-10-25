module Test.Spago.Build where

import Test.Prelude

import Data.Foldable (fold)
import Data.String as String
import Node.Platform as Platform
import Node.Process as Process
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Spago.Paths as Paths
import Test.Spago.Build.BuildInfo as BuildInfo
import Test.Spago.Build.Monorepo as Monorepo
import Test.Spago.Build.Pedantic as Pedantic
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Assertions.String (shouldContain)

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "build" do

    Spec.it "builds successfully" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess

    Spec.it "builds successfully a solver-only package" \{ spago } -> do
      spago [ "init", "--name", "aaa", "--use-solver" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess

    Spec.it "passes options to purs" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "build", "--purs-args", "--verbose-errors", "--purs-args", "--comments" ] >>= shouldBeSuccess

    Spec.it "can't pass the --json-errors flag to purs" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa" ] >>= shouldBeSuccess
      spago [ "build", "--purs-args", "--json-errors" ] >>= shouldBeFailureErr (fixture "json-errors-err.txt")

    Spec.it "can use a different output folder" \{ spago, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "build", "--output", "myOutput" ] >>= shouldBeSuccess
      FS.exists (testCwd </> "myOutput") `Assert.shouldReturn` true
      FS.exists (testCwd </> "output") `Assert.shouldReturn` false

    Spec.it "can build with a local custom package set" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.unlink (testCwd </> "spago.yaml")
      FS.copyFile { src: fixture "local-package-set-config.yaml", dst: testCwd </> "spago.yaml" }
      FS.copyFile { src: fixture "local-package-set.json", dst: testCwd </> "local-package-set.json" }
      spago [ "build" ] >>= shouldBeSuccess

    Spec.it "can build with a local custom package set in a parent directory" \{ spago, fixture, testCwd } -> do
      FS.copyFile { src: fixture "local-package-set.json", dst: testCwd </> "local-package-set.json" }
      let subdir = testCwd </> "subdir"
      FS.mkdirp subdir
      Paths.chdir subdir
      spago [ "init" ] >>= shouldBeSuccess
      FS.unlink $ subdir </> "spago.yaml"
      FS.copyFile { src: fixture "local-package-set-config2.yaml", dst: subdir </> "spago.yaml" }
      spago [ "build" ] >>= shouldBeSuccess

    Spec.it "there's only one output folder in a monorepo" \{ spago, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp $ testCwd </> "subpackage" </> "src"
      FS.mkdirp $ testCwd </> "subpackage" </> "test"
      FS.writeTextFile (testCwd </> "subpackage" </> "src" </> "Main.purs") (Init.srcMainTemplate "Subpackage.Main")
      FS.writeTextFile (testCwd </> "subpackage" </> "test" </> "Main.purs") (Init.testMainTemplate "Subpackage.Test.Main")
      FS.writeYamlFile Config.configCodec (testCwd </> "subpackage" </> "spago.yaml")
        ( Init.defaultConfig
            { name: mkPackageName "subpackage"
            , testModuleName: "Subpackage.Test.Main"
            , withWorkspace: Nothing
            }
        )
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "-p", "subpackage" ] >>= shouldBeSuccess
      FS.exists (testCwd </> "output") `Assert.shouldReturn` true
      FS.exists (testCwd </> "subpackage" </> "output") `Assert.shouldReturn` false

    Spec.it "--strict causes build to fail if there are warnings" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let srcMain = testCwd </> "src" </> "Main.purs"
      FS.unlink srcMain
      FS.copyFile
        { src: fixture "check-strict.purs"
        , dst: srcMain
        }
      spago [ "build", "--strict" ] >>= shouldBeFailure

    Spec.it "having 'strict: true' in a package config fails the build if there are warnings" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        srcMain = testCwd </> "src" </> "Main.purs"
        spagoYaml = testCwd </> "spago.yaml"
      FS.unlink srcMain
      FS.copyFile
        { src: fixture "check-strict.purs"
        , dst: srcMain
        }
      FS.unlink spagoYaml
      FS.copyFile
        { src: fixture "check-strict.yaml"
        , dst: spagoYaml
        }
      spago [ "build" ] >>= shouldBeFailure

    Spec.it "respects the --censor-stats flag" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "--censor-stats" ] >>= shouldBeSuccessErr (fixture "censor-stats-output.txt")

    Spec.it "should censor warnings with given errorcode and prefix messsage" \{ spago, fixture, testCwd } -> do
      FS.copyTree { src: fixture "build/censor-warnings", dst: testCwd </> "." }

      let
        remainingWarningPath = [ escapePathInErrMsg [ "src", "Main.purs:5:1" ] ]
        filteredWarningPath =
          [ escapePathInErrMsg [ "src", "Main.purs:3:1" ]
          , escapePathInErrMsg [ "src", "Main.purs:10:6" ]
          ]

        shouldHaveWarning = assertWarning remainingWarningPath true
        shouldNotHaveWarning = assertWarning filteredWarningPath false

      spago [ "build" ] >>= check
        { stdout: mempty
        , stderr: fold <<< flap [ shouldHaveWarning, shouldNotHaveWarning ]
        , result: isRight
        }

    Spec.describe "lockfile" do
      Spec.it "building with a lockfile doesn't need the Registry repo" \{ spago, fixture, testCwd } -> do
        spago [ "init", "--name", "aaa", "--package-set", "33.0.0" ] >>= shouldBeSuccess
        spago [ "build" ] >>= shouldBeSuccess
        -- Check that we have written the lockfile
        checkFixture (testCwd </> "spago.lock") (fixture "spago.lock")
        -- Then remove the registry repo
        rmRf Paths.registryPath
        -- And check that we can still build
        spago [ "build" ] >>= shouldBeSuccess
        -- And that we still don't have the registry
        FS.exists Paths.registryPath `Assert.shouldReturn` false

      Spec.it "using the --pure flag does not refresh the lockfile" \{ spago, fixture, testCwd } -> do
        spago [ "init", "--name", "aaa", "--package-set", "33.0.0" ] >>= shouldBeSuccess
        spago [ "build" ] >>= shouldBeSuccess
        -- Check that we have written the lockfile
        checkFixture (testCwd </> "spago.lock") (fixture "spago.lock")
        -- Update the config
        let
          conf = Init.defaultConfig
            { name: mkPackageName "aaa"
            , testModuleName: "Test.Main"
            , withWorkspace: Just { setVersion: Just $ mkVersion "33.0.0" }
            }
        FS.writeYamlFile Config.configCodec (testCwd </> "spago.yaml")
          (conf { package = conf.package # map (\pkg -> pkg { dependencies = pkg.dependencies <> mkDependencies [ "maybe" ] }) })
        -- Check that building with --pure does not refresh the lockfile
        spago [ "build", "--pure" ] >>= shouldBeSuccess
        checkFixture (testCwd </> "spago.lock") (fixture "spago.lock")

      Spec.it "lockfile is refreshed when the local package set changes" \{ spago, fixture, testCwd } -> do
        FS.copyTree { src: fixture "build/local-package-set-lockfile", dst: testCwd </> "." }
        spago [ "build" ] >>= shouldBeSuccess
        checkFixture (testCwd </> "spago.lock") (fixture "build/local-package-set-lockfile/spago.lock.old")
        FS.moveSync { src: testCwd </> "local-package-set.json", dst: testCwd </> "old-package-set.json" }
        FS.moveSync { src: testCwd </> "new-package-set.json", dst: testCwd </> "local-package-set.json" }
        spago [ "build" ] >>= shouldBeSuccess
        checkFixture (testCwd </> "spago.lock") (fixture "build/local-package-set-lockfile/spago.lock.new")

    Spec.it "compiles with the specified backend" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          { name: mkPackageName "subpackage"
          , testModuleName: "Test.Main"
          , withWorkspace: Just
              { setVersion: Just $ mkVersion "0.0.1"
              }
          }
      FS.writeYamlFile Config.configCodec (testCwd </> "spago.yaml")
        (conf { workspace = conf.workspace # map (_ { backend = Just { cmd: "echo", args: Just [ "hello" ] } }) })

      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run" ] >>= shouldBeSuccessErr (fixture "alternate-backend-output.txt")

      -- We also make sure that no js files are produced, only corefn
      FS.exists (testCwd </> "output" </> "Main" </> "index.js") `Assert.shouldReturn` false
      FS.exists (testCwd </> "output" </> "Main" </> "corefn.json") `Assert.shouldReturn` true

    Spec.it "passing the --codegen flag to purs fails" \{ spago, fixture } -> do
      spago [ "init", "--name", "7368613235362d68766258694c614d517a3667747a58725778" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "--purs-args", "--codegen", "--purs-args", "corefn" ] >>= shouldBeFailureErr (fixture "codegen-opt.txt")

    Spec.it "passing the --ensure-ranges flag without package selection adds ranges to root package when it exists" \{ spago, testCwd } -> do
      spago [ "init", "--package-set", "0.0.1" ] >>= shouldBeSuccess
      spago [ "build", "--ensure-ranges" ] >>= shouldBeSuccess
      spagoYaml <- FS.readTextFile (testCwd </> "spago.yaml")
      spagoYaml `shouldContain` "- prelude: \">=6.0.1 <7.0.0\""

    Spec.it "failed build with many warnings and --json-errors does not truncate output" \{ spago, fixture, testCwd } -> do
      FS.copyTree { src: fixture "build/json-truncated-many-warnings", dst: testCwd </> "." }
      spago [ "build", "--json-errors" ] >>= shouldBeFailureOutput case Process.platform of
        Just Platform.Win32 -> fixture "build/json-truncated-many-warnings/warnings-windows.json"
        _ -> fixture "build/json-truncated-many-warnings/warnings.json"

    Spec.it "building with old-format config files works, as well as migrating them" \{ spago, fixture, testCwd } -> do
      FS.copyTree { src: fixture "build/migrate-config", dst: testCwd </> "." }
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccessErr (fixture "build/migrate-config/unmigrated-warning.txt")
      spago [ "build", "--migrate" ] >>= shouldBeSuccessErr (fixture "build/migrate-config/migrating-output.txt")
      spago [ "build" ] >>= shouldBeSuccessErr (fixture "build/migrate-config/migrated-output.txt")
      checkFixture (testCwd </> "spago.yaml") (fixture "build/migrate-config/migrated-spago.yaml")

    Spec.it "#1148: outputs errors and warnings after build" \{ spago, fixture, testCwd } -> do
      let
        shouldBeSuccessErr' = checkOutputsWithPathSeparatorPatchErr isRight
        shouldBeFailureErr' = checkOutputsWithPathSeparatorPatchErr isLeft

        checkOutputsWithPathSeparatorPatchErr result expectedFixture =
          checkOutputs'
            { stdoutFile: Nothing
            , stderrFile: Just $ fixture expectedFixture
            , result
            , sanitize:
                String.trim
                  >>> String.replaceAll (String.Pattern $ "src\\") (String.Replacement "src/")
                  >>> String.replaceAll (String.Pattern $ "\r\n") (String.Replacement "\n")
            }

      FS.copyTree { src: fixture "build/1148-warnings-diff-errors", dst: testCwd </> "." }

      Paths.chdir $ testCwd </> "errors"
      spago [ "install" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeFailureErr' "build/1148-warnings-diff-errors/errors/expected-stderr.txt"

      Paths.chdir $ testCwd </> "warnings"
      spago [ "install" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccessErr' "build/1148-warnings-diff-errors/warnings/expected-stderr.txt"

    Pedantic.spec

    Monorepo.spec

    BuildInfo.spec
