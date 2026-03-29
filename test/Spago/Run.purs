module Test.Spago.Run where

import Test.Prelude

import Data.String as String
import Spago.FS as FS
import Spago.Path as Path
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions.String (shouldContain)

spec :: CommandLocks -> Spec Unit
spec locks = Spec.parallel $ Spec.around (withBuildLock locks) do
  Spec.describe "run" do

    Spec.it "works at all" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run" ] >>= shouldBeSuccessOutput (fixture "run-output.txt")

    Spec.it "can pass stdin to the application" \{ spago, spago', fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.copyTree { src: fixture "spago-run-stdin.purs", dst: testCwd </> "src" </> "Main.purs" }
      spago [ "install", "node-buffer", "node-streams", "node-process", "node-event-emitter" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago' (StdinWrite "wut") [ "run" ] >>= shouldBeSuccessOutput (fixture "run-passthrough.txt")

    Spec.it "can pass arguments, uses spago.yaml args as fallback, and explicit args override config" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.copyTree { src: fixture "spago-run-args.purs", dst: testCwd </> "src" </> "Main.purs" }
      spago [ "install", "node-process", "arrays" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess

      -- can pass arguments to the application
      spago [ "run", "hello", "world" ] >>= shouldBeSuccessOutput (fixture "run-args-output.txt")

      -- args in spago.yaml should be used as the fallback args
      editSpagoYaml' (testCwd </> "spago.yaml") \config ->
        config { package = config.package <#> \p -> p { run = Just { main: Nothing, execArgs: Just [ "hello", "world" ] } } }
      spago [ "run" ] >>= shouldBeSuccessOutput (fixture "run-args-output.txt")

      -- explicit args has more priority than args in spago.yaml
      spago [ "run", "bye", "world" ] >>= shouldBeSuccessOutput (fixture "run-args-output2.txt")

    Spec.it "works with special characters in path (apostrophe, spaces, brackets)" \{ spagoIn, fixture, testCwd } -> do
      -- Test apostrophe - "Tim's Test" should become package "tims-test"
      let dir1 = testCwd </> "Tim's Test"
      FS.mkdirp dir1
      spagoIn dir1 [ "init" ] >>= shouldBeSuccess
      config1 <- FS.readTextFile (dir1 </> "spago.yaml")
      config1 `shouldContain` "name: tims-test"
      spagoIn dir1 [ "build" ] >>= shouldBeSuccess
      spagoIn dir1 [ "run" ] >>= shouldBeSuccessOutput (fixture "run-output.txt")

      -- Test spaces - "My Project Dir" should become "my-project-dir"
      let dir2 = testCwd </> "My Project Dir"
      FS.mkdirp dir2
      spagoIn dir2 [ "init" ] >>= shouldBeSuccess
      config2 <- FS.readTextFile (dir2 </> "spago.yaml")
      config2 `shouldContain` "name: my-project-dir"
      spagoIn dir2 [ "build" ] >>= shouldBeSuccess
      spagoIn dir2 [ "run" ] >>= shouldBeSuccessOutput (fixture "run-output.txt")

      -- Test multiple special characters - "Test #1 (dev)" should become "test-1-dev"
      let dir3 = testCwd </> "Test #1 (dev)"
      FS.mkdirp dir3
      spagoIn dir3 [ "init" ] >>= shouldBeSuccess
      config3 <- FS.readTextFile (dir3 </> "spago.yaml")
      config3 `shouldContain` "name: test-1-dev"
      spagoIn dir3 [ "build" ] >>= shouldBeSuccess
      spagoIn dir3 [ "run" ] >>= shouldBeSuccessOutput (fixture "run-output.txt")

    Spec.it "init fails gracefully when directory name has no valid characters" \{ spagoIn, fixture, testCwd } -> do
      let dir = testCwd </> "###"
      FS.mkdirp dir
      spagoIn dir [ "init" ] >>= checkOutputs'
        { stdoutFile: Nothing
        , stderrFile: Just (fixture "init-invalid-dirname.txt")
        , result: isLeft
        , sanitize:
            String.trim
              >>> String.replaceAll (String.Pattern $ Path.toRaw dir) (String.Replacement "...")
        }
