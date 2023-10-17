module Test.Spago.Build where

import Test.Prelude

import Data.Array as Array
import Data.Map as Map
import Node.FS.Aff as FSA
import Node.Path as Path
import Node.Process as Process
import Spago.Command.Init as Init
import Spago.Core.Config (configCodec)
import Spago.Core.Config as Config
import Spago.FS as FS
import Test.Spago.Build.Polyrepo as BuildPolyrepo
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
      spago [ "build", "--purs-args", "--verbose-errors", "--purs-args", "--json-errors" ] >>= shouldBeSuccess

    Spec.it "can use a different output folder" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "build", "--output", "myOutput" ] >>= shouldBeSuccess
      FS.exists "myOutput" `Assert.shouldReturn` true
      FS.exists "output" `Assert.shouldReturn` false

    -- TODO: no-install flag
    -- Spec.it "does not install packages when passed the --no-install flag" \{ spago } -> do
    --   spago [ "init" ] >>= shouldBeSuccess
    --   spago [ "build", "--no-install" ] >>= shouldBeFailure
    --   spago [ "install" ] >>= shouldBeSuccess
    --   spago [ "build", "--no-install" ] >>= shouldBeSuccess

    Spec.it "can build with a local custom package set" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FSA.unlink "spago.yaml"
      FS.copyFileSync { src: fixture "local-package-set-config.yaml", dst: "spago.yaml" }
      FS.copyFileSync { src: fixture "local-package-set.json", dst: "local-package-set.json" }
      spago [ "build" ] >>= shouldBeSuccess

    Spec.it "can build with a local custom package set in a parent directory" \{ spago, fixture } -> do
      FS.copyFileSync { src: fixture "local-package-set.json", dst: "local-package-set.json" }
      FS.mkdirp "subdir"
      liftEffect $ Process.chdir "subdir"
      spago [ "init" ] >>= shouldBeSuccess
      FSA.unlink "spago.yaml"
      FS.copyFileSync { src: fixture "local-package-set-config2.yaml", dst: "spago.yaml" }
      spago [ "build" ] >>= shouldBeSuccess

    Spec.it "there's only one output folder in a monorepo" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp (Path.concat [ "subpackage", "src" ])
      FS.mkdirp (Path.concat [ "subpackage", "test" ])
      FS.writeTextFile (Path.concat [ "subpackage", "src", "Main.purs" ]) (Init.srcMainTemplate "Subpackage.Main")
      FS.writeTextFile (Path.concat [ "subpackage", "test", "Main.purs" ]) (Init.testMainTemplate "Subpackage.Test.Main")
      FS.writeYamlFile Config.configCodec (Path.concat [ "subpackage", "spago.yaml" ])
        ( Init.defaultConfig
            { name: mkPackageName "subpackage"
            , testModuleName: "Subpackage.Test.Main"
            , withWorkspace: Nothing
            }
        )
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "-p", "subpackage" ] >>= shouldBeSuccess
      FS.exists "output" `Assert.shouldReturn` true
      FS.exists (Path.concat [ "subpackage", "output" ]) `Assert.shouldReturn` false

    Spec.describe "pedantic packages" do

      let
        modifyPackageConfig f = do
          content <- FS.readYamlDocFile configCodec "spago.yaml"
          case content of
            Left err ->
              Assert.fail $ "Failed to decode spago.yaml file\n" <> err
            Right { yaml: config } ->
              FS.writeYamlFile configCodec "spago.yaml" $ f config

        addPedanticPackagesToSrc = modifyPackageConfig \config ->
          config
            { package = config.package <#> \r -> r
                { build = Just
                    { pedantic_packages: Just true
                    , strict: Nothing
                    , censor_project_warnings: Nothing
                    }
                }
            }
        addPedanticPackagesToTest = modifyPackageConfig \config ->
          config
            { package = config.package <#> \r -> r
                { test = Just
                    { main: "Test.Main"
                    , pedantic_packages: Just true
                    , strict: Nothing
                    , censor_test_warnings: Nothing
                    , dependencies: maybe (Config.Dependencies Map.empty) _.dependencies r.test
                    , execArgs: r.test >>= _.execArgs
                    }
                }
            }

      Spec.describe "fails when imports from transitive dependencies" do

        Spec.describe "appear in the source package and" do

          let
            setupSrcTransitiveTests spago installConsoleAndEffect = do
              spago [ "init", "--name", "7368613235362d34312f4e59746b7869335477336d33414d72" ] >>= shouldBeSuccess
              spago [ "install", "maybe" ] >>= shouldBeSuccess
              when installConsoleAndEffect do
                spago [ "install", "--test-deps", "console", "effect" ] >>= shouldBeSuccess
              FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) $ Array.intercalate "\n"
                [ "module Main where"
                , "import Prelude"
                , "import Data.Maybe as Maybe"
                , "import Effect as Effect"
                , "import Effect.Console as Console"
                , "import Control.Alt as Alt"
                , "main = unit"
                ]
              -- get rid of "Compiling ..." messages and other compiler warnings
              spago [ "build" ] >>= shouldBeSuccess

          Spec.it "passed --pedantic-packages CLI flag" \{ spago, fixture } -> do
            setupSrcTransitiveTests spago true
            spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "check-direct-import-transitive-dependency.txt")

          Spec.it "package config has 'pedantic_packages: true'" \{ spago, fixture } -> do
            setupSrcTransitiveTests spago false
            addPedanticPackagesToSrc
            spago [ "build" ] >>= shouldBeFailureErr (fixture "check-direct-import-transitive-dependency.txt")

        Spec.describe "appear in the test package and" do

          let
            setupTestTransitiveTests spago = do
              spago [ "init", "--name", "7368613235362d34312f4e59746b7869335477336d33414d72" ] >>= shouldBeSuccess
              spago [ "install", "maybe" ] >>= shouldBeSuccess
              FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) $ Array.intercalate "\n"
                [ "module Main where"
                , "import Prelude"
                , "import Data.Maybe as Maybe"
                , "import Effect as Effect"
                , "import Effect.Console as Console"
                , "main = unit"
                ]
              FS.writeTextFile (Path.concat [ "test", "Test", "Main.purs" ]) $ Array.intercalate "\n"
                [ "module Test.Main where"
                , "import Prelude"
                , "import Data.Maybe as Maybe"
                , "import Effect as Effect"
                , "import Effect.Console as Console"
                , "import Control.Alt as Alt"
                , "main = unit"
                ]
              -- get rid of "Compiling ..." messages and other compiler warnings
              spago [ "build" ] >>= shouldBeSuccess

          Spec.it "passed --pedantic-packages CLI flag" \{ spago, fixture } -> do
            setupTestTransitiveTests spago
            spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "check-direct-import-transitive-dependency-test.txt")

          Spec.it "package config has 'pedantic_packages: true'" \{ spago, fixture } -> do
            setupTestTransitiveTests spago
            addPedanticPackagesToTest
            spago [ "build" ] >>= shouldBeFailureErr (fixture "check-direct-import-transitive-dependency-test.txt")

      Spec.describe "fails with warnings about unused dependencies" do

        Spec.describe "in a source package when" do

          let
            setupSrcUnusedDeps spago installConsoleAndEffect = do
              spago [ "init", "--name", "7368613235362d2f444a2b4f56375435646a59726b53586548" ] >>= shouldBeSuccess
              when installConsoleAndEffect do
                spago [ "install", "--test-deps", "console", "effect" ] >>= shouldBeSuccess
              FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) "module Main where\nimport Prelude\nmain = unit"
              -- get rid of "Compiling ..." messages and other compiler warnings
              spago [ "build" ] >>= shouldBeSuccess

          Spec.it "passing --pedantic-packages CLI flag" \{ spago, fixture } -> do
            setupSrcUnusedDeps spago true
            spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "check-unused-dependency.txt")

          Spec.it "package config has 'pedantic_packages: true'" \{ spago, fixture } -> do
            setupSrcUnusedDeps spago false
            addPedanticPackagesToSrc
            spago [ "build" ] >>= shouldBeFailureErr (fixture "check-unused-dependency.txt")

        Spec.describe "in a test package when" do

          let
            setupTestUnusedDeps spago = do
              spago [ "init", "--name", "7368613235362d2f444a2b4f56375435646a59726b53586548" ] >>= shouldBeSuccess
              spago [ "install", "--test-deps", "newtype" ] >>= shouldBeSuccess
              FS.writeTextFile (Path.concat [ "test", "Test", "Main.purs" ]) $ Array.intercalate "\n"
                [ "module Test.Main where"
                , "import Prelude"
                , "import Effect as Effect"
                , "import Effect.Console as Console"
                , "main = unit"
                ]
              -- get rid of "Compiling ..." messages and other compiler warnings
              spago [ "build" ] >>= shouldBeSuccess

          Spec.it "passing --pedantic-packages CLI flag" \{ spago, fixture } -> do
            setupTestUnusedDeps spago
            spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "check-unused-test-dependency.txt")

          Spec.it "package's test config has 'pedantic_packages: true'" \{ spago, fixture } -> do
            setupTestUnusedDeps spago
            addPedanticPackagesToTest
            spago [ "build" ] >>= shouldBeFailureErr (fixture "check-unused-test-dependency.txt")

        Spec.describe "in both the source and test packages when" do

          let
            setupUnusedDeps spago = do
              spago [ "init", "--name", "7368613235362d2f444a2b4f56375435646a59726b53586548" ] >>= shouldBeSuccess
              spago [ "install", "prelude", "effect", "console" ] >>= shouldBeSuccess
              spago [ "install", "--test-deps", "prelude", "effect", "console" ] >>= shouldBeSuccess
              FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) "module Main where\nimport Prelude\nmain = unit"
              FS.writeTextFile (Path.concat [ "test", "Test", "Main.purs" ]) "module Test.Main where\nimport Prelude\nmain = unit"
              -- get rid of "Compiling ..." messages and other compiler warnings
              spago [ "build" ] >>= shouldBeSuccess

          Spec.it "passing --pedantic-packages CLI flag" \{ spago, fixture } -> do
            setupUnusedDeps spago
            spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "check-unused-source-and-test-dependency.txt")

          Spec.it "package config has 'pedantic_packages: true'" \{ spago, fixture } -> do
            setupUnusedDeps spago
            addPedanticPackagesToSrc
            addPedanticPackagesToTest
            spago [ "build" ] >>= shouldBeFailureErr (fixture "check-unused-source-and-test-dependency.txt")

      {-
      Goal:
        Src
          - remove either - unused
          - install newtype - (transitive dep of either) as it will be needed once either is removed
        Test
          - remove tuples - unused
          - install either - (implicit dep of source package, but need to add it here once removed from source)
      -}
      Spec.describe "fails to build and reports deduplicated src and test unused/transitive dependenciess when" do
        let
          addPedanticPackages = do
            modifyPackageConfig \config ->
              config
                { package = config.package <#> \r -> r
                    { build = Just
                        { pedantic_packages: Just true
                        , strict: r.build >>= _.strict
                        , censor_project_warnings: r.build >>= _.censor_project_warnings
                        }
                    , test = Just
                        { main: "Test.Main"
                        , pedantic_packages: Just true
                        , strict: r.test >>= _.strict
                        , censor_test_warnings: r.test >>= _.censor_test_warnings
                        , dependencies: maybe (Config.Dependencies Map.empty) _.dependencies r.test
                        , execArgs: r.test >>= _.execArgs
                        }
                    }
                }

          setupUnusedAndTransitiveDeps spago = do
            -- Since we need to edit `spago.yaml` and other files generated by `spago init`,
            -- may as well just create those files by hand.
            FS.writeYamlFile configCodec "spago.yaml" $ mkPackageAndWorkspaceConfig
              { package: { packageName: "foo", srcDependencies: [ "prelude", "control", "either" ] }
              , workspace: { setVersion: Just $ mkVersion "0.0.1" }
              }
              [ configAddTestMain
              , configAddTestDependencies [ "tuples" ]
              ]

            FS.mkdirp "src"
            FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) $ Array.intercalate "\n"
              [ "module Main where "
              , ""
              , "import Prelude"
              , "import Data.Newtype as Newtype"
              , "import Control.Alt as Alt"
              , ""
              , "foo :: Int"
              , "foo = 1"
              ]

            FS.mkdirp $ Path.concat [ "test", "Test" ]
            FS.writeTextFile (Path.concat [ "test", "Test", "Main.purs" ]) $ Array.intercalate "\n"
              [ "module Test.Main where "
              , ""
              , "import Prelude"
              , "import Data.Newtype (class Newtype)"
              , "import Data.Either (Either(..))"
              , ""
              , "newtype Bar = Bar Int"
              , "derive instance Newtype Bar _"
              , ""
              , "foo :: Either Bar Int"
              , "foo = Right 1"
              ]
            -- get rid of "Compiling ..." messages and other compiler warnings
            spago [ "build" ] >>= shouldBeSuccess

        Spec.it "passing --pedantic-packages" \{ spago, fixture } -> do
          setupUnusedAndTransitiveDeps spago
          spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "check-pedantic-packages.txt")

        Spec.it "package config has `pedantic_packages: true` in both `build` and `test`" \{ spago, fixture } -> do
          setupUnusedAndTransitiveDeps spago
          addPedanticPackages
          spago [ "build" ] >>= shouldBeFailureErr (fixture "check-pedantic-packages.txt")

    Spec.it "--strict causes build to fail if there are warnings" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let srcMain = Path.concat [ "src", "Main.purs" ]
      FSA.unlink srcMain
      FS.copyFile
        { src: fixture "check-strict.purs"
        , dst: srcMain
        }
      spago [ "build", "--strict" ] >>= shouldBeFailure

    Spec.it "having 'strict: true' in a package config fails the build if there are warnings" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        srcMain = Path.concat [ "src", "Main.purs" ]
        spagoYaml = "spago.yaml"
      FSA.unlink srcMain
      FS.copyFile
        { src: fixture "check-strict.purs"
        , dst: srcMain
        }
      FSA.unlink spagoYaml
      FS.copyFile
        { src: fixture "check-strict.yaml"
        , dst: spagoYaml
        }
      spago [ "build" ] >>= shouldBeFailure

    Spec.it "compiles with the specified backend" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          { name: mkPackageName "subpackage"
          , testModuleName: "Test.Main"
          , withWorkspace: Just
              { setVersion: Just $ mkVersion "0.0.1"
              }
          }
      FS.writeYamlFile Config.configCodec "spago.yaml"
        (conf { workspace = conf.workspace # map (_ { backend = Just { cmd: "echo", args: Just [ "hello" ] } }) })

      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run" ] >>= shouldBeSuccessErr (fixture "alternate-backend-output.txt")

      -- We also make sure that no js files are produced, only corefn
      FS.exists "output/Main/index.js" `Assert.shouldReturn` false
      FS.exists "output/Main/corefn.json" `Assert.shouldReturn` true

    Spec.it "passing the --codegen flag to purs fails" \{ spago, fixture } -> do
      spago [ "init", "--name", "7368613235362d68766258694c614d517a3667747a58725778" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "--purs-args", "--codegen", "--purs-args", "corefn" ] >>= shouldBeFailureErr (fixture "codegen-opt.txt")

    Spec.it "passing the --ensure-ranges flag without package selection adds ranges to root package when it exists" \{ spago } -> do
      spago [ "init", "--package-set", "0.0.1" ] >>= shouldBeSuccess
      spago [ "build", "--ensure-ranges" ] >>= shouldBeSuccess
      spagoYaml <- FS.readTextFile "spago.yaml"
      spagoYaml `shouldContain` "- prelude: \">=6.0.1 <7.0.0\""

    BuildPolyrepo.spec

-- Spec.it "runs a --before command" \{ spago } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   let dumpFile = "testOutput"
--   spago [ "build", "--before", "echo before>> " <> dumpFile ] >>= shouldBeSuccess
--   test <- FS.readTextFile dumpFile
--   test `Assert.shouldEqual` "before"

-- Spec.it "runs a --then command" \{ spago } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   let dumpFile = "testOutput"
--   spago [ "build", "--then", "echo then>> " <> dumpFile, "--else", "echo else>> " <> dumpFile ] >>= shouldBeSuccess
--   test <- FS.readTextFile dumpFile
--   test `Assert.shouldEqual` "then"

-- Spec.it "runs a --before command before a --then command" \{ spago } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   let dumpFile = "testOutput"
--   spago [ "build", "--before", "echo before>> " <> dumpFile, "--then", "echo then>> " <> dumpFile ] >>= shouldBeSuccess
--   test <- FS.readTextFile dumpFile
--   test `Assert.shouldEqual` "before\nthen"

-- Spec.it "runs an --else command if there is an error in the build" \{ spago } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   let dumpFile = "testOutput"
--   FS.writeTextFile "src/Main.purs" "Invalid Purescript code"
--   spago [ "build", "--then", "echo then>> " <> dumpFile, "--else", "echo else>> " <> dumpFile ] >>= shouldBeFailure
--   test <- FS.readTextFile dumpFile
--   test `Assert.shouldEqual` "else"

-- Spec.it "runs an --else command if there is an error in the run file" \{ spago } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   spago [ "install", "exceptions" ] >>= shouldBeSuccess
--   let dumpFile = "testOutput"
--   FS.writeTextFile "src/Main.purs" "module Main where\nimport Effect.Exception\nmain = throw \"error\""
--   spago [ "run", "--else", "echo else>> " <> dumpFile ] >>= shouldBeFailure
--   test <- FS.readTextFile dumpFile
--   test `Assert.shouldEqual` "else"

-- Spec.it "runs multiple commands in order" \{ spago } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   let dumpFile = "testOutput"
--   spago [ "build", "--before", "echo before1>> " <> dumpFile, "--before", "echo before2>> " <> dumpFile, "--then", "echo then1>> " <> dumpFile, "--then", "echo then2>> " <> dumpFile ] >>= shouldBeSuccess
--   test <- FS.readTextFile dumpFile
--   test `Assert.shouldEqual` "before1\nbefore2\nthen1\nthen2"

-- Spec.it "fails the build if a --before command fails" \{ spago } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   spago [ "build", "--before", "exit 1" ] >>= shouldBeFailure

-- Spec.it "fails the build if a --then command fails" \{ spago } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   spago [ "build", "--then", "exit 1" ] >>= shouldBeFailure

-- Spec.it "still fails the build if an --else command fails" \{ spago } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   FS.writeTextFile "src/Main.purs" "Invalid Purescript code"
--   spago [ "build", "--else", "exit 1" ] >>= shouldBeFailure
