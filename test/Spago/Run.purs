module Test.Spago.Run where

import Test.Prelude

import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec

cp :: forall m. MonadAff m => String -> String -> m Unit
cp from to = do
  str <- FS.readTextFile from
  FS.writeTextFile to str

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "run" do

    Spec.it "works at all" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run" ] >>= shouldBeSuccessOutput (fixture "run-output.txt")

    Spec.it "can pass stdin to the application" \{ spago, spago', fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      cp (fixture "spago-run-stdin.purs") "src/Main.purs"
      spago [ "install", "node-buffer", "node-streams", "node-process", "node-event-emitter" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago' (StdinWrite "wut") [ "run" ] >>= shouldBeSuccessOutput (fixture "run-passthrough.txt")

    Spec.it "can pass arguments to the application" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      cp (fixture "spago-run-args.purs") "src/Main.purs"
      spago [ "install", "node-process", "arrays" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run", "hello" , "world" ] >>= shouldBeSuccessOutput (fixture "run-args-output.txt")

    Spec.it "args in spago.yaml should be used as the fallback args" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      cp (fixture "spago-run-args.purs") "src/Main.purs"
      cp (fixture "spago-args.yaml") "spago.yaml"
      spago [ "install", "node-process", "arrays" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run" ] >>= shouldBeSuccessOutput (fixture "run-args-output.txt")

    Spec.it "explicit args has more priority than args in spago.yaml" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      cp (fixture "spago-args.yaml") "spago.yaml"
      cp (fixture "spago-run-args.purs") "src/Main.purs"
      spago [ "install", "node-process", "arrays" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "run", "bye" , "world" ] >>= shouldBeSuccessOutput (fixture "run-args-output2.txt")
