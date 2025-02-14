module Test.Spago.Transfer (spec) where

import Test.Prelude

import Spago.FS as FS
import Spago.Path as Path
import Test.Spago.Publish (doTheGitThing)
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "transfer" do

    Spec.it "fails if the publish config is not specified" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaaa" ] >>= shouldBeSuccess
      spago [ "registry", "transfer", "--offline", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeFailureErr (fixture "publish/transfer/no-publish-config.txt")

    Spec.it "fails if the config does not specify an owner" \{ spago, fixture, testCwd } -> do
      FS.copyFile { src: fixture "publish/basic.yaml", dst: testCwd </> "spago.yaml" }
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "registry", "transfer", "--offline", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeFailureErr (fixture "publish/transfer/no-owner.txt")

    Spec.it "fails if the git tree is not clean" \{ spago, fixture, testCwd } -> do
      FS.copyFile { src: fixture "publish/basic.yaml", dst: testCwd </> "spago.yaml" }
      spago [ "auth", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeSuccess
      spago [ "registry", "transfer", "--offline", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeFailureErr (fixture "publish/transfer/no-git.txt")

    Spec.it "fails if the package has never been published before" \{ spago, fixture, testCwd } -> do
      FS.copyFile { src: fixture "publish/basic.yaml", dst: testCwd </> "spago.yaml" }
      spago [ "auth", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "registry", "transfer", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeFailureErr (fixture "publish/transfer/never-published.txt")

    Spec.it "fails if the new repo location is the same as the current one in the registry" \{ spago, fixture, testCwd } -> do
      FS.copyFile { src: fixture "publish/transfer/aff.yaml", dst: testCwd </> "spago.yaml" }
      spago [ "auth", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "registry", "transfer", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeFailureErr (fixture "publish/transfer/same-location.txt")

    Spec.it "fails if can't find the private key" \{ spago, fixture, testCwd } -> do
      FS.copyFile { src: fixture "publish/transfer/aff-new-location.yaml", dst: testCwd </> "spago.yaml" }
      spago [ "auth", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "registry", "transfer", "-i", (Path.toRaw $ fixture "publish/no-key") ] >>= shouldBeFailureErr (fixture "publish/transfer/no-key.txt")

    Spec.it "fails if running with --offline" \{ spago, fixture, testCwd } -> do
      FS.copyFile { src: fixture "publish/transfer/aff-new-location.yaml", dst: testCwd </> "spago.yaml" }
      spago [ "auth", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "registry", "transfer", "--offline", "-i", (Path.toRaw $ fixture "publish/key") ] >>= shouldBeFailureErr (fixture "publish/transfer/offline.txt")
