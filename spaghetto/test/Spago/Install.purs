module Test.Spago.Install where

import Test.Prelude

import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "install" do

    -- Spec.it "Subsequent installs should succeed after failed install" \{ spago, checkFixture } -> do

    --   spago [ "init" ] >>= shouldBeSuccess
    --   -- Run `install` once and kill it soon to simulate failure
    --   runFor 5000 "spago" [ "install", "-j", "3" ]
    --   -- Sleep for some time, as the above might take time to cleanup old processes
    --   threadDelay 1000000
    --   spago [ "install", "-j", "10" ] >>= shouldBeSuccess

    Spec.it "Spago should warn that config was not changed, when trying to install package already present in project dependencies" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "install" ] >>= shouldBeSuccess
      spago [ "fetch", "effect" ] >>= shouldBeSuccessErr (fixture "spago-install-existing-dep-stderr.txt")

-- Spec.it "Spago should strip 'purescript-' prefix and give warning if package without prefix is present in package set" \{ spago, checkFixture } -> do

--   spago [ "init" ] >>= shouldBeSuccess
--   spago [ "install", "safe-coerce" ] >>= shouldBeSuccess
--   spago [ "install", "purescript-newtype" ] >>= shouldBeSuccessStderr "spago-install-purescript-prefix-stderr.txt"
--   -- dep added without "purescript-" prefix
--   checkFileHasInfix "spago.dhall" "\"newtype\""

-- Spec.it "Spago should be able to add dependencies" \{ spago, fixture } -> do
--   spago [ "init", "--name", "aaa" ] >>= shouldBeSuccess
--   spago [ "install", "foreign" ] >>= shouldBeSuccess
--   checkFixture "spago.yaml" (fixture "spago-install-success.dhall")

-- Spec.it "Spago should not add dependencies that are not in the package set" \{ spago, fixture } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   spago [ "install", "foo", "bar" ] >>= shouldBeFailureStderr (fixture "missing-dependencies.txt")
--   checkFixture "spago.yaml" (fixture "spago-install-failure.dhall")

-- Spec.it "Spago should not allow circular dependencies" \{ spago, fixture } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   writeTextFile "spago.dhall" "{- Welcome to a Spago project!  You can edit this file as you like.  -} { name = \"my-project\" , dependencies = [ \"effect\", \"console\", \"a\", \"b\" ] , packages = ./packages.dhall // { a = { version = \"a1\", dependencies = [\"b\"], repo = \"https://github.com/fake/fake.git\" }, b = { version = \"b1\", dependencies = [\"a\"], repo = \"https://github.com/fake/fake.git\" } } }"
--   spago [ "install" ] >>= shouldBeFailureStderr "circular-dependencies.txt"

-- Spec.it "Spago should be able to install a package in the set from a commit hash" \{ spago, fixture } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   -- The commit for `either` is for the `v5.0.0` release
--   writeTextFile "packages.dhall" "let pkgs = ./packagesBase.dhall in pkgs // { either = pkgs.either // { version = \"c1a1af35684f10eecaf6ac7d38dbf6bd48af2ced\" } }"
--   spago [ "install", "either" ] >>= shouldBeSuccess

-- Spec.it "Spago should be able to install a package version by branch name with / in it" \{ spago, checkFixture } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   mv "packages.dhall" "packagesBase.dhall"
--   writeTextFile "packages.dhall" "let pkgs = ./packagesBase.dhall in pkgs // { metadata_ = { dependencies = [\"prelude\"], repo = \"https://github.com/spacchetti/purescript-metadata.git\", version = \"spago-test/branch-with-slash\" }}"
--   spago [ "install", "metadata_" ] >>= shouldBeSuccess

-- Spec.it "Spago should be able to install a package not in the set from a commit hash" \{ spago, checkFixture } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   mv "packages.dhall" "packagesBase.dhall"
--   writeTextFile "packages.dhall" "let pkgs = ./packagesBase.dhall in pkgs // { spago = { dependencies = [\"prelude\"], repo = \"https://github.com/purescript/spago.git\", version = \"cbdbbf8f8771a7e43f04b18cdefffbcb0f03a990\" }}"
--   spago [ "install", "spago" ] >>= shouldBeSuccess

-- Spec.it "Spago should not be able to install a package from a not-existing commit hash" \{ spago, checkFixture } -> do
--   spago [ "init" ] >>= shouldBeSuccess
--   mv "packages.dhall" "packagesBase.dhall"
--   writeTextFile "packages.dhall" "let pkgs = ./packagesBase.dhall in pkgs // { spago = { dependencies = [\"prelude\"], repo = \"https://github.com/purescript/spago.git\", version = \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" }}"
--   spago [ "install", "spago" ] >>= shouldBeFailure

{-
    Spec.it "Spago should be able to update dependencies in a sub-package" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp "packages/foo"
      -- TODO: withCwd spago init
      FS.writeTextFile "packages/foo/spago.yaml" "./spago.dhall // {dependencies = [\"prelude\"]}"
      spago [ "-p", "foo", "install", "either" ] >>= shouldBeSuccess
      checkFixture "packages/foo/spago.yaml" (fixture "spago-subpackage-install-success.yaml")

    Spec.it "Spago should fail when the alternate config file doesn't exist" \{ spago, checkFixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "install", "-x", "test.dhall" ] >>= shouldBeFailureStderr "alternate-config-missing.txt"

    Spec.it "Spago should not change the alternative config if it does not change dependencies" \{ spago, checkFixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      writeTextFile "alternative2.dhall" "./spago.dhall // { sources = [ \"src/**/*.purs\" ] }\n"
      spago [ "-x", "alternative2.dhall", "install", "either" ] >>= shouldBeSuccess
      spago [ "-x", "alternative2.dhall", "install", "either" ] >>= shouldBeSuccessStderr "alternative2install-stderr.txt"
      checkFixture "alternative2.dhall"

    Spec.it "Spago should install successfully when there are local dependencies sharing the same packages.dhall" \{ spago, checkFixture } -> do

      -- Create local 'lib-a' package that depends on lib-c
      mkdir "lib-a"
      cd "lib-a"
      spago [ "init" ] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-a\", dependencies = [\"console\", \"effect\", \"prelude\", \"lib-c\"], packages = ../packages.dhall }"
      cd ".."

      -- Create local 'lib-b' package that has its dependencies in a separate file
      mkdir "lib-b"
      cd "lib-b"
      spago [ "init" ] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-b\", dependencies = ./spago-deps.dhall, packages = ../packages.dhall }"
      writeTextFile "spago-deps.dhall" "[\"console\", \"effect\", \"prelude\"]"
      cd ".."

      -- Create local 'lib-c' package
      mkdir "lib-c"
      cd "lib-c"
      spago [ "init" ] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-c\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ../packages.dhall }"
      cd ".."

      -- Create 'app' package that depends on 'lib-a' and 'lib-b'
      spago [ "init" ] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" "{ name = \"app\", dependencies = [\"console\", \"effect\", \"prelude\", \"lib-a\", \"lib-b\"], packages = ./packages.dhall }"
      packageDhall <- readTextFile "packages.dhall"
      writeTextFile "packages.dhall" $ packageDhall <> " // { lib-a = ./lib-a/spago.dhall as Location, lib-b = ./lib-b/spago.dhall as Location, lib-c = ./lib-c/spago.dhall as Location }"

      spago [ "install" ] >>= shouldBeSuccess

    Spec.it "adds a hash to the package set when importing it from a URL" \{ spago, fixture } -> do
      pure unit
-}
