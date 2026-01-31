module Test.Spago.Run where

import Test.Prelude

import Data.String as String
import Spago.FS as FS
import Spago.Path as Path
import Spago.Paths as Paths
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions.String (shouldContain)

spec :: Spec Unit
spec = Spec.around withTempDir do
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

    Spec.it "can pass arguments to the application" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.copyTree { src: fixture "spago-run-args.purs", dst: testCwd </> "src" </> "Main.purs" }
      spago [ "install", "node-process", "arrays" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run", "hello" , "world" ] >>= shouldBeSuccessOutput (fixture "run-args-output.txt")

    Spec.it "args in spago.yaml should be used as the fallback args" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.copyTree { src: fixture "spago-run-args.purs", dst: testCwd </> "src" </> "Main.purs" }
      FS.copyTree { src: fixture "spago-args.yaml", dst: testCwd </> "spago.yaml" }
      spago [ "install", "node-process", "arrays" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run" ] >>= shouldBeSuccessOutput (fixture "run-args-output.txt")

    Spec.it "explicit args has more priority than args in spago.yaml" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.copyTree { src: fixture "spago-args.yaml", dst: testCwd </> "spago.yaml" }
      FS.copyTree { src: fixture "spago-run-args.purs", dst: testCwd </> "src" </> "Main.purs" }
      spago [ "install", "node-process", "arrays" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run", "bye" , "world" ] >>= shouldBeSuccessOutput (fixture "run-args-output2.txt")

    Spec.it "works with special characters in path (apostrophe, spaces, brackets)" \{ spago, fixture, testCwd } -> do
      -- Test apostrophe - "Tim's Test" should become package "tims-test"
      let dir1 = testCwd </> "Tim's Test"
      FS.mkdirp dir1
      Paths.chdir dir1
      spago [ "init" ] >>= shouldBeSuccess
      config1 <- FS.readTextFile (dir1 </> "spago.yaml")
      config1 `shouldContain` "name: tims-test"
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run" ] >>= shouldBeSuccessOutput (fixture "run-output.txt")

      -- Test spaces - "My Project Dir" should become "my-project-dir"
      let dir2 = testCwd </> "My Project Dir"
      FS.mkdirp dir2
      Paths.chdir dir2
      spago [ "init" ] >>= shouldBeSuccess
      config2 <- FS.readTextFile (dir2 </> "spago.yaml")
      config2 `shouldContain` "name: my-project-dir"
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run" ] >>= shouldBeSuccessOutput (fixture "run-output.txt")

      -- Test multiple special characters - "Test #1 (dev)" should become "test-1-dev"
      let dir3 = testCwd </> "Test #1 (dev)"
      FS.mkdirp dir3
      Paths.chdir dir3
      spago [ "init" ] >>= shouldBeSuccess
      config3 <- FS.readTextFile (dir3 </> "spago.yaml")
      config3 `shouldContain` "name: test-1-dev"
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run" ] >>= shouldBeSuccessOutput (fixture "run-output.txt")

    Spec.it "init fails gracefully when directory name has no valid characters" \{ spago, fixture, testCwd } -> do
      let dir = testCwd </> "###"
      FS.mkdirp dir
      Paths.chdir dir
      spago [ "init" ] >>= checkOutputs'
        { stdoutFile: Nothing
        , stderrFile: Just (fixture "init-invalid-dirname.txt")
        , result: isLeft
        , sanitize:
            String.trim
              >>> String.replaceAll (String.Pattern $ Path.toRaw dir) (String.Replacement "...")
        }
