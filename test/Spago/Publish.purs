module Test.Spago.Publish (spec) where

import Test.Prelude

import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RF
import Node.Platform as Platform
import Node.Process as Process
import Spago.Cmd as Cmd
import Spago.FS as FS
import Spago.Path as Path
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "publish" do

    Spec.it "fails if the version bounds are not specified" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaaa" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish/no-bounds.txt")

    Spec.it "fails if the publish config is not specified" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaaa" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "fetch", "--ensure-ranges" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish/no-config.txt")

    Spec.it "fails if the git tree is not clean" \{ spago, fixture, testCwd } -> do
      FS.copyFile { src: fixture "publish/basic.yaml", dst: testCwd </> "spago.yaml" }
      FS.mkdirp $ testCwd </> "src"
      FS.copyFile { src: fixture "publish/basic.purs", dst: testCwd </> "src/Main.purs" }
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish/no-git.txt")

    Spec.it "fails if the module is called Main" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaaa" ] >>= shouldBeSuccess
      FS.unlink $ testCwd </> "spago.yaml"
      FS.copyFile { src: fixture "publish/basic.yaml", dst: testCwd </> "spago.yaml" }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr case Process.platform of
        Just Platform.Win32 -> fixture "publish/main-win.txt"
        _ -> fixture "publish/main.txt"

    Spec.it "fails if the publish repo location is not among git remotes" \{ spago, fixture, testCwd } -> do
      FS.copyFile { src: fixture "publish/basic.yaml", dst: testCwd </> "spago.yaml" }
      FS.mkdirp $ testCwd </> "src"
      FS.copyFile { src: fixture "publish/basic.purs", dst: testCwd </> "src/Main.purs" }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      git [ "remote", "set-url", "origin", "git@github.com:purescript/bbb.git" ] -- TODO check this is a Right?
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish/invalid-location.txt")

    Spec.it "fails if a core dependency is not in the registry" \{ spago, fixture, testCwd } -> do
      FS.copyTree { src: fixture "publish/extra-package-core", dst: testCwd }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "fetch" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish/extra-package-core-dependency.txt")

    Spec.it "can get a package ready to publish" \{ spago, fixture, testCwd } -> do
      FS.copyFile { src: fixture "publish/basic.yaml", dst: testCwd </> "spago.yaml" }
      FS.mkdirp $ testCwd </> "src"
      FS.copyFile { src: fixture "publish/basic.purs", dst: testCwd </> "src/Main.purs" }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      -- It will fail because it can't hit the registry, but the fixture will check that everything else is ready
      spago [ "fetch" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish/ready.txt")

    Spec.it "allows to publish with a test dependency not in the registry" \{ spago, fixture, testCwd } -> do
      FS.copyTree { src: fixture "publish/extra-package-test", dst: testCwd }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "fetch" ] >>= shouldBeSuccess
      spago [ "publish", "--offline" ] >>= shouldBeFailureErr (fixture "publish/ready.txt")

    Spec.it "#1307 allows other non-published projects to reference local project in the workspace" \{ spago, fixture, testCwd } -> do
      FS.copyTree { src: fixture "publish/1307-publish-dependencies", dst: testCwd }
      spago [ "build" ] >>= shouldBeSuccess
      doTheGitThing
      spago [ "fetch" ] >>= shouldBeSuccess
      spago [ "publish", "-p", "root", "--offline" ] >>= shouldBeFailureErr (fixture "publish/1307-publish-dependencies/expected-stderr.txt")

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

  Spec.it "#1110 installs versions of packages that are returned by the registry solver, but not present in cache" \{ spago, fixture, testCwd } -> do
    let
      shouldBeFailureErr' file = checkOutputs'
        { stdoutFile: Nothing
        , stderrFile: Just file
        , result: isLeft
        , sanitize:
            String.trim
              >>> String.replaceAll (String.Pattern "\\") (String.Replacement "/")
              >>> String.replaceAll (String.Pattern "\r\n") (String.Replacement "\n")
              >>> Regex.replace buildOrderRegex "[x of 3] Compiling module-name"
        }

      -- We have to ignore lines like "[1 of 3] Compiling Effect.Console" when
      -- comparing output, because the compiler will always compile in
      -- different order, depending on how the system resources happened to
      -- align at the moment of the test run.
      buildOrderRegex = unsafeFromRight $ Regex.regex
        "\\[\\d of 3\\] Compiling (Effect\\.Console|Effect\\.Class\\.Console|Lib)"
        RF.global

    FS.copyTree { src: fixture "publish/1110-solver-different-version", dst: testCwd }
    spago [ "build" ] >>= shouldBeSuccess
    doTheGitThing
    spago [ "fetch" ] >>= shouldBeSuccess

    -- The local `spago.yaml` specifies `console: 6.0.0` in `extraPackages`,
    -- so that's what should be in local cache after running `fetch`.
    -- Importantly, `console-6.1.0` should not be there yet.
    FS.exists (testCwd </> ".spago/p/console-6.0.0") >>= (_ `shouldEqual` true)
    FS.exists (testCwd </> ".spago/p/console-6.1.0") >>= (_ `shouldEqual` false)

    spago [ "publish", "--offline" ] >>= shouldBeFailureErr' (fixture "publish/1110-solver-different-version/expected-stderr.txt")

    -- When `publish` runs, it uses the registry solver, which returns
    -- `console-6.1.0` version, so `publish` should fetch that into local
    -- cache and build with it.
    FS.exists (testCwd </> ".spago/p/console-6.1.0") >>= (_ `shouldEqual` true)

    -- Now screw up the `console-6.1.0` package in the local cache, so that it
    -- doesn't compile anymore, and check that the relevant compile error
    -- happens on publish.
    FS.unlink $ testCwd </> ".spago/p/console-6.1.0/src/Effect/Console.js"
    rmRf $ testCwd </> ".spago/p/console-6.1.0/output"
    spago [ "publish", "--offline" ] >>= shouldBeFailureErr' (fixture "publish/1110-solver-different-version/failure-stderr.txt")

  Spec.describe "#1060 auto-filling the `publish.location` field" do
    let
      prepareProject spago fixture testCwd = do
        FS.copyTree { src: fixture "publish/1060-autofill-location/project", dst: testCwd }
        spago [ "build" ] >>= shouldBeSuccess
        doTheGitThing
        spago [ "fetch" ] >>= shouldBeSuccess

    Spec.it "happens for root package" \{ fixture, spago, testCwd } -> do
      prepareProject spago fixture testCwd
      spago [ "publish", "-p", "aaa", "--offline" ] >>=
        shouldBeFailureErr (fixture "publish/1060-autofill-location/scenario-root/expected-stderr.txt")
      checkFixture (testCwd </> "spago.yaml")
        (fixture "publish/1060-autofill-location/scenario-root/expected-spago.yaml")

    Spec.it "errors out for non-root package" \{ fixture, spago, testCwd } -> do
      prepareProject spago fixture testCwd
      spago [ "publish", "-p", "bbb", "--offline" ] >>=
        shouldBeFailureErr (fixture "publish/1060-autofill-location/scenario-subdir/expected-stderr.txt")

    Spec.it "errors out for nested non-root package" \{ fixture, spago, testCwd } -> do
      prepareProject spago fixture testCwd
      spago [ "publish", "-p", "ccc", "--offline" ] >>=
        shouldBeFailureErr (fixture "publish/1060-autofill-location/scenario-nested-subdir/expected-stderr.txt")

    Spec.it "errors out when not a GitHub remote" \{ fixture, spago, testCwd } -> do
      prepareProject spago fixture testCwd
      git [ "remote", "set-url", "origin", "https://not.git-hub.net/foo/bar.git" ]
      spago [ "publish", "-p", "aaa", "--offline" ] >>=
        shouldBeFailureErr (fixture "publish/1060-autofill-location/scenario-non-github/expected-stderr.txt")
      checkFixture (testCwd </> "spago.yaml")
        (fixture "publish/1060-autofill-location/scenario-non-github/expected-spago.yaml")

    Spec.it "prints error when no origin remote" \{ fixture, spago, testCwd } -> do
      prepareProject spago fixture testCwd
      git [ "remote", "remove", "origin" ]
      git [ "remote", "add", "upstream", "git@github.com:foo/bar.git" ]
      spago [ "publish", "-p", "aaa", "--offline" ] >>=
        shouldBeFailureErr (fixture "publish/1060-autofill-location/scenario-no-origin/expected-stderr.txt")
      checkFixture (testCwd </> "spago.yaml")
        (fixture "publish/1060-autofill-location/project/spago.yaml")

doTheGitThing :: Aff Unit
doTheGitThing = do
  git [ "init" ]
  git [ "config", "user.name", "test-user" ]
  git [ "config", "user.email", "test-user@aol.com" ]
  git [ "config", "commit.gpgSign", "false" ]
  git [ "config", "tag.gpgSign", "false" ]
  git [ "add", "." ]
  git [ "commit", "-m", "first" ]
  git [ "tag", "v0.0.1" ]
  git [ "remote", "add", "origin", "git@github.com:purescript/aaa.git" ]

git :: Array String -> Aff Unit
git = git' Nothing

git' :: Maybe GlobalPath -> Array String -> Aff Unit
git' cwd args =
  Cmd.exec (Path.global "git") args
    (Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, pipeStdin = StdinNewPipe, cwd = cwd })
    >>= shouldBeSuccess
