module Test.Spago.Build where

import Test.Prelude

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
        addPedanticPackagesToPackageConfig { src, test } = do
          content <- FS.readYamlDocFile configCodec "spago.yaml"
          case content of
            Left err ->
              Assert.fail $ "Failed to decode spago.yaml file\n" <> err
            Right { yaml: config } ->
              FS.writeYamlFile configCodec "spago.yaml" $ config
                { package = config.package <#> \r -> r
                    { build = Just
                        { pedantic_packages: Just src
                        , strict: Nothing
                        , censor_project_warnings: Nothing
                        }
                    , test = Just
                        { main: "Test.Main"
                        , pedantic_packages: Just test
                        , strict: Nothing
                        , censor_test_warnings: Nothing
                        , dependencies: Config.Dependencies $ Map.fromFoldable $ map (flip Tuple Nothing <<< mkPackageName) [ "newtype" ]
                        , execArgs: Nothing
                        }
                    }
                }

      Spec.describe "fails when there are imports from transitive dependencies and" do
        let
          setupSrcTransitiveTests spago = do
            spago [ "init", "--name", "7368613235362d34312f4e59746b7869335477336d33414d72" ] >>= shouldBeSuccess
            spago [ "install", "maybe" ] >>= shouldBeSuccess
            FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) "module Main where\nimport Prelude\nimport Data.Maybe\nimport Control.Alt\nmain = unit"
            spago [ "build" ] >>= shouldBeSuccess

        Spec.it "passed --pedantic-packages CLI flag" \{ spago, fixture } -> do
          setupSrcTransitiveTests spago
          spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "check-direct-import-transitive-dependency.txt")

        Spec.it "package config has 'pedantic_packages: true'" \{ spago, fixture } -> do
          setupSrcTransitiveTests spago
          addPedanticPackagesToPackageConfig { src: true, test: false }
          spago [ "build" ] >>= shouldBeFailureErr (fixture "check-direct-import-transitive-dependency.txt")

      Spec.describe "warns about unused dependencies when" do
        let
          setupSrcUnusedDeps spago = do
            spago [ "init", "--name", "7368613235362d2f444a2b4f56375435646a59726b53586548" ] >>= shouldBeSuccess
            FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) "module Main where\nimport Prelude\nmain = unit"
            spago [ "build" ] >>= shouldBeSuccess

        Spec.it "passing --pedantic-packages CLI flag" \{ spago, fixture } -> do
          setupSrcUnusedDeps spago
          spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "check-unused-dependency.txt")

        Spec.it "package config has 'pedantic_packages: true'" \{ spago, fixture } -> do
          setupSrcUnusedDeps spago
          addPedanticPackagesToPackageConfig { src: true, test: false }
          spago [ "build" ] >>= shouldBeFailureErr (fixture "check-unused-dependency.txt")

        Spec.it "package's test config has 'pedantic_packages: true' and test code has unused dependencies" \{ spago, fixture } -> do
          spago [ "init", "--name", "7368613235362d2f444a2b4f56375435646a59726b53586548" ] >>= shouldBeSuccess
          FS.writeTextFile (Path.concat [ "test", "Test", "Main.purs" ]) "module Test.Main where\nimport Prelude\nmain = unit"
          spago [ "build" ] >>= shouldBeSuccess
          addPedanticPackagesToPackageConfig { src: false, test: true }
          spago [ "build" ] >>= shouldBeFailureErr (fixture "check-unused-test-dependency.txt")

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
