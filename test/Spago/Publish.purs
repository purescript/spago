module Test.Spago.Publish (spec) where

import Test.Prelude

import Node.Platform as Platform
import Node.Process as Process
import Spago.Cmd as Cmd
import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "publish" do

    Spec.it "fails if the version bounds are not specified" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaaa" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish-no-bounds.txt")

    Spec.it "fails if the publish config is not specified" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaaa" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "fetch", "--ensure-ranges" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish-no-config.txt")

    Spec.it "fails if the git tree is not clean" \{ spago, fixture } -> do
      FS.copyFile { src: fixture "spago-publish.yaml", dst: "spago.yaml" }
      FS.mkdirp "src"
      FS.copyFile { src: fixture "publish.purs", dst: "src/Main.purs" }
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish-no-git.txt")

    Spec.it "fails if the module is called Main" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaaa" ] >>= shouldBeSuccess
      FS.unlink "spago.yaml"
      FS.copyFile { src: fixture "spago-publish.yaml", dst: "spago.yaml" }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr case Process.platform of
        Just Platform.Win32 -> fixture "publish-main-win.txt"
        _ -> fixture "publish-main.txt"

    Spec.it "fails if the publish repo location is not among git remotes" \{ spago, fixture } -> do
      FS.copyFile { src: fixture "spago-publish.yaml", dst: "spago.yaml" }
      FS.mkdirp "src"
      FS.copyFile { src: fixture "publish.purs", dst: "src/Main.purs" }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      git [ "remote", "set-url", "origin", "git@github.com:purescript/bbb.git" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish-invalid-location.txt")

    Spec.it "fails if a core dependency is not in the registry" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "publish/extra-package-core", dst: "." }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "fetch" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish-extra-package-core-dependency.txt")

    Spec.it "can get a package ready to publish" \{ spago, fixture } -> do
      FS.copyFile { src: fixture "spago-publish.yaml", dst: "spago.yaml" }
      FS.mkdirp "src"
      FS.copyFile { src: fixture "publish.purs", dst: "src/Main.purs" }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      -- It will fail because it can't hit the registry, but the fixture will check that everything else is ready
      spago [ "fetch" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish.txt")

    Spec.it "allows to publish with a test dependency not in the registry" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "publish/extra-package-test", dst: "." }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "fetch" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish.txt")

doTheGitThing :: Aff Unit
doTheGitThing = do
  git [ "init" ] >>= shouldBeSuccess
  git [ "config", "user.name", "test-user" ] >>= shouldBeSuccess
  git [ "config", "user.email", "test-user@aol.com" ] >>= shouldBeSuccess
  git [ "config", "commit.gpgSign", "false" ] >>= shouldBeSuccess
  git [ "config", "tag.gpgSign", "false" ] >>= shouldBeSuccess
  git [ "add", "." ] >>= shouldBeSuccess
  git [ "commit", "-m", "first" ] >>= shouldBeSuccess
  git [ "tag", "v0.0.1" ] >>= shouldBeSuccess
  git [ "remote", "add", "origin", "git@github.com:purescript/aaa.git" ] >>= shouldBeSuccess

git :: Array String -> Aff (Either ExecResult ExecResult)
git args = Cmd.exec "git" args
  $ Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, pipeStdin = StdinNewPipe }
