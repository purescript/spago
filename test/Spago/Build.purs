module Test.Spago.Build where

import Test.Prelude

import Node.Path as Path
import Registry.Version as Version
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

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

    Spec.it "there's only one output folder in a monorepo" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp (Path.concat [ "subpackage", "src" ])
      FS.mkdirp (Path.concat [ "subpackage", "test" ])
      FS.writeTextFile (Path.concat [ "subpackage", "src", "Main.purs" ]) (Init.srcMainTemplate "Subpackage.Main")
      FS.writeTextFile (Path.concat [ "subpackage", "test", "Main.purs" ]) (Init.testMainTemplate "Subpackage.Test.Main")
      FS.writeYamlFile Config.configCodec (Path.concat [ "subpackage", "spago.yaml" ])
        ( Init.defaultConfig
            { name: mkPackageName "subpackage"
            , setVersion: Nothing
            , testModuleName: "Subpackage.Test.Main"
            , withWorkspace: false
            }
        )
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "-p", "subpackage" ] >>= shouldBeSuccess
      FS.exists "output" `Assert.shouldReturn` true
      FS.exists (Path.concat [ "subpackage", "output" ]) `Assert.shouldReturn` false

    Spec.it "fails when there are imports from transitive dependencies and --pedantic-packages is passed" \{ spago, fixture } -> do
      spago [ "init", "--name", "7368613235362d34312f4e59746b7869335477336d33414d72" ] >>= shouldBeSuccess
      spago [ "install", "maybe" ] >>= shouldBeSuccess
      FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) "module Main where\nimport Prelude\nimport Data.Maybe\nimport Control.Alt\nmain = unit"
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "check-direct-import-transitive-dependency.txt")

    Spec.it "--pedantic-packages also warns about unused dependencies" \{ spago, fixture } -> do
      spago [ "init", "--name", "7368613235362d2f444a2b4f56375435646a59726b53586548" ] >>= shouldBeSuccess
      FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) "module Main where\nimport Prelude\nmain = unit"
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "check-unused-dependency.txt")

    Spec.it "compiles with the specified backend" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          { name: mkPackageName "subpackage"
          , setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1"
          , testModuleName: "Test.Main"
          , withWorkspace: true
          }
      FS.writeYamlFile Config.configCodec "spago.yaml"
        (conf { workspace = conf.workspace # map (_ { backend = Just { cmd: "echo", args: Just [ "hello" ] } }) })

      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run" ] >>= shouldBeSuccessErr (fixture "alternate-backend-output.txt")

    Spec.it "passing the --codegen flag to purs fails" \{ spago, fixture } -> do
      spago [ "init", "--name", "7368613235362d68766258694c614d517a3667747a58725778" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "--purs-args", "--codegen", "--purs-args", "corefn" ] >>= shouldBeFailureErr (fixture "codegen-opt.txt")

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
