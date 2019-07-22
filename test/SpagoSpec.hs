module SpagoSpec (spec) where

import           Control.Concurrent (threadDelay)
import           Data.Foldable      (for_)
import qualified Data.Text          as Text
import           Prelude            hiding (FilePath)
import qualified System.IO.Temp     as Temp
import           Test.Hspec         (Spec, around_, describe, it, shouldBe)
import           Turtle             (ExitCode (..), cd, cp, decodeString, fromText, mkdir, mktree,
                                     mv, readTextFile, rm, testdir, writeTextFile, shell, empty)
import           Utils              (checkFixture, readFixture, runFor, shouldBeFailure,
                                     shouldBeFailureOutput, shouldBeSuccess, shouldBeSuccessOutput,
                                     spago, withCwd)


setup :: IO () -> IO ()
setup cmd = do
  Temp.withTempDirectory "test/" "spago-test" $ \temp -> do
    -- print ("Running in " <> temp)
    withCwd (decodeString temp) cmd

spec :: Spec
spec = around_ setup $ do

  describe "spago init" $ do

    it "Spago should have set up a project" $ do

      spago ["init"] >>= shouldBeSuccess

    it "Spago should refuse to overwrite an existing project without -f" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["init"] >>= shouldBeFailure

    it "Spago should not overwrite files when initing a project" $ do

      mktree "src"
      writeTextFile "src/Main.purs" "Something"
      spago ["init"] >>= shouldBeSuccess
      readTextFile "src/Main.purs" >>= (`shouldBe` "Something")

    it "Spago should always succeed in doing init with force" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["init", "-f"] >>= shouldBeSuccess

    it "Spago should import config from psc-package" $ do

      writeTextFile "psc-package.json" "{ \"name\": \"aaa\", \"depends\": [ \"prelude\" ], \"set\": \"foo\", \"source\": \"bar\" }"
      spago ["init"] >>= shouldBeSuccess
      cp "spago.dhall" "spago-psc-success.dhall"
      checkFixture "spago-psc-success.dhall"

    it "Spago should not import dependencies that are not in the package-set" $ do

      writeTextFile "psc-package.json" "{ \"name\": \"aaa\", \"depends\": [ \"prelude\", \"foo\", \"bar\" ], \"set\": \"foo\", \"source\": \"bar\" }"
      spago ["init", "-f"] >>= shouldBeSuccess
      cp "spago.dhall" "spago-psc-failure.dhall"
      checkFixture "spago-psc-failure.dhall"


  describe "spago install" $ do

    it "Subsequent installs should succeed after failed install" $ do

      spago ["init"] >>= shouldBeSuccess
      -- Run `install` once and kill it soon to simulate failure
      runFor 5000 "spago" ["install", "-j", "3"]
      -- Sleep for some time, as the above might take time to cleanup old processes
      threadDelay 1000000
      spago ["install", "-j", "10"] >>= shouldBeSuccess

    it "Spago should be able to add dependencies" $ do

      writeTextFile "psc-package.json" "{ \"name\": \"aaa\", \"depends\": [ \"prelude\" ], \"set\": \"foo\", \"source\": \"bar\" }"
      spago ["init"] >>= shouldBeSuccess
      spago ["install", "-j10", "simple-json", "foreign"] >>= shouldBeSuccess
      mv "spago.dhall" "spago-install-success.dhall"
      checkFixture "spago-install-success.dhall"

    it "Spago should not add dependencies that are not in the package set" $ do

      writeTextFile "psc-package.json" "{ \"name\": \"aaa\", \"depends\": [ \"prelude\" ], \"set\": \"foo\", \"source\": \"bar\" }"
      spago ["init"] >>= shouldBeSuccess
      spago ["install", "foo", "bar"] >>= shouldBeFailureOutput "missing-dependencies.txt"
      mv "spago.dhall" "spago-install-failure.dhall"
      checkFixture "spago-install-failure.dhall"

    it "Spago should not allow circular dependencies" $ do

      writeTextFile "psc-package.json" "{ \"name\": \"aaa\", \"depends\": [ \"prelude\" ], \"set\": \"foo\", \"source\": \"bar\" }"
      spago ["init"] >>= shouldBeSuccess
      writeTextFile "spago.dhall" "{- Welcome to a Spago project!  You can edit this file as you like.  -} { name = \"my-project\" , dependencies = [ \"effect\", \"console\", \"psci-support\", \"a\", \"b\" ] , packages = ./packages.dhall // { a = { version = \"a1\", dependencies = [\"b\"], repo = \"https://github.com/fake/fake.git\" }, b = { version = \"b1\", dependencies = [\"a\"], repo = \"https://github.com/fake/fake.git\" } } }"
      spago ["install"] >>= shouldBeFailureOutput "circular-dependencies.txt"

    it "Spago should be able to install a package in the set from a commit hash" $ do

      spago ["init"] >>= shouldBeSuccess
      mv "packages.dhall" "packagesBase.dhall"
      writeTextFile "packages.dhall" "let pkgs = ./packagesBase.dhall in pkgs // { simple-json = pkgs.simple-json // { version = \"d45590f493d68baae174b2d3062d502c0cc4c265\" } }"
      spago ["install", "simple-json"] >>= shouldBeSuccess

    it "Spago should be able to install a package version by branch name with / in it" $ do

      spago ["init"] >>= shouldBeSuccess
      mv "packages.dhall" "packagesBase.dhall"
      writeTextFile "packages.dhall" "let pkgs = ./packagesBase.dhall in pkgs // { metadata_ = { dependencies = [\"prelude\"], repo = \"https://github.com/spacchetti/purescript-metadata.git\", version = \"spago-test/branch-with-slash\" }}"
      spago ["install", "metadata_"] >>= shouldBeSuccess

    it "Spago should be able to install a package not in the set from a commit hash" $ do

      spago ["init"] >>= shouldBeSuccess
      mv "packages.dhall" "packagesBase.dhall"
      writeTextFile "packages.dhall" "let pkgs = ./packagesBase.dhall in pkgs // { spago = { dependencies = [\"prelude\"], repo = \"https://github.com/spacchetti/spago.git\", version = \"cbdbbf8f8771a7e43f04b18cdefffbcb0f03a990\" }}"
      spago ["install", "spago"] >>= shouldBeSuccess

    it "Spago should not be able to install a package from a not-existing commit hash" $ do

      spago ["init"] >>= shouldBeSuccess
      mv "packages.dhall" "packagesBase.dhall"
      writeTextFile "packages.dhall" "let pkgs = ./packagesBase.dhall in pkgs // { spago = { dependencies = [\"prelude\"], repo = \"https://github.com/spacchetti/spago.git\", version = \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" }}"
      spago ["install", "spago"] >>= shouldBeFailure

    it "Spago should install successfully when there are local dependencies sharing the same packages.dhall" $ do

      -- Create local 'lib-a' package that depends on lib-c
      mkdir "lib-a"
      cd "lib-a"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-a\", dependencies = [\"console\", \"effect\", \"prelude\", \"lib-c\"], packages = ../packages.dhall }"
      cd ".."

      -- Create local 'lib-b' package that has its dependencies in a separate file
      mkdir "lib-b"
      cd "lib-b"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-b\", dependencies = ./spago-deps.dhall, packages = ../packages.dhall }"
      writeTextFile "spago-deps.dhall" "[\"console\", \"effect\", \"prelude\"]"
      cd ".."

      -- Create local 'lib-c' package
      mkdir "lib-c"
      cd "lib-c"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-c\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ../packages.dhall }"
      cd ".."

      -- Create 'app' package that depends on 'lib-a' and 'lib-b'
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" "{ name = \"app\", dependencies = [\"console\", \"effect\", \"prelude\", \"lib-a\", \"lib-b\"], packages = ./packages.dhall }"
      packageDhall <- readTextFile "packages.dhall"
      writeTextFile "packages.dhall" $ packageDhall <> " // { lib-a = ./lib-a/spago.dhall as Location, lib-b = ./lib-b/spago.dhall as Location, lib-c = ./lib-c/spago.dhall as Location }"

      spago ["install"] >>= shouldBeSuccess

  describe "spago sources" $ do

    it "Spago should print both dependencies and project sources" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["sources"] >>= shouldBeSuccessOutput "sources-output.txt"

  describe "spago build" $ do

    it "Spago should build successfully" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["build"] >>= shouldBeSuccess

    it "Spago should pass options to purs" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["build", "--", "-o", "myOutput"] >>= shouldBeSuccess
      testdir "myOutput" >>= (`shouldBe` True)

    it "Spago should build successfully with sources included from custom path" $ do

      spago ["init"] >>= shouldBeSuccess
      mkdir "another_source_path"
      mv "src/Main.purs" "another_source_path/Main.purs"
      spago ["build", "--path", "another_source_path/*.purs"] >>= shouldBeSuccess

    it "Spago should not install packages when passing the --no-install flag" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["build", "--no-install"] >>= shouldBeFailure
      spago ["install"] >>= shouldBeSuccess
      spago ["build", "--no-install"] >>= shouldBeSuccess

    it "Spago should add sources to config when key is missing" $ do

      configV1 <- readFixture "spago-configV1.dhall"
      spago ["init"] >>= shouldBeSuccess
      -- Replace initial config with the old config format (without 'sources')
      mv "spago.dhall" "spago-old.dhall"
      writeTextFile "spago.dhall" configV1

      spago ["install"] >>= shouldBeSuccess
      mv "spago.dhall" "spago-configV2.dhall"
      checkFixture "spago-configV2.dhall"

  describe "spago test" $ do

    it "Spago should test successfully" $ do

      spago ["init"] >>= shouldBeSuccess
      -- Note: apparently purs starts caching the compiled modules only after three builds
      spago ["build"] >>= shouldBeSuccess
      spago ["build"] >>= shouldBeSuccess
      spago ["test"] >>= shouldBeSuccessOutput "test-output.txt"


  describe "spago package-set-upgrade" $ do

    it "Spago should migrate package-sets from src/packages.dhall to the released one" $ do

      spago ["init"] >>= shouldBeSuccess
      mv "packages.dhall" "packages-expected.dhall"
      packages <- readTextFile "packages-expected.dhall"
      let [packageSetUrl]
            = map Text.strip
            $ filter (not . null . Text.breakOnAll "https://github.com/purescript/package-sets")
            $ Text.lines packages
      writeTextFile "packages.dhall" "https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190713/src/packages.dhall sha256:906af79ba3aec7f429b107fd8d12e8a29426db8229d228c6f992b58151e2308e"
      spago ["package-set-upgrade"] >>= shouldBeSuccess
      newPackages <- fmap Text.strip $ readTextFile "packages.dhall"
      newPackages `shouldBe` packageSetUrl


  describe "spago run" $ do

    it "Spago should run successfully" $ do

      spago ["init"] >>= shouldBeSuccess
      -- Note: apparently purs starts caching the compiled modules only after three builds
      spago ["build"] >>= shouldBeSuccess
      spago ["build"] >>= shouldBeSuccess

      shell "psa --version" empty >>= \case
        ExitSuccess -> spago ["-v", "run"] >>= shouldBeSuccessOutput "run-output.txt"
        ExitFailure _ ->  spago ["-v", "run"] >>= shouldBeSuccessOutput "run-output-psa-not-installed.txt"

    it "Spago should be able to not use `psa`" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["--no-psa", "build"] >>= shouldBeSuccess
      spago ["--no-psa", "build"] >>= shouldBeSuccess
      spago ["-v", "--no-psa", "run"] >>= shouldBeSuccessOutput "run-no-psa.txt"


  describe "spago bundle" $ do

    it "Spago should fail but should point to the replacement command" $ do

      spago ["bundle", "--to", "bundle.js"] >>= shouldBeFailureOutput "bundle-output.txt"


  describe "spago bundle-app" $ do

    it "Spago should bundle successfully" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["bundle-app", "--to", "bundle-app.js"] >>= shouldBeSuccess
      checkFixture "bundle-app.js"


  describe "spago make-module" $ do

    it "Spago should fail but should point to the replacement command" $ do

      spago ["make-module", "--to", "make-module.js"] >>= shouldBeFailureOutput "make-module-output.txt"


  describe "spago bundle-module" $ do

    it "Spago should successfully make a module" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["build"] >>= shouldBeSuccess
      -- Now we don't remove the output folder, but we pass the `--no-build`
      -- flag to skip rebuilding (i.e. we are counting on the previous command
      -- to have built stuff for us)
      spago ["bundle-module", "--to", "bundle-module.js", "--no-build"] >>= shouldBeSuccess
      checkFixture "bundle-module.js"

  describe "spago list-packages" $ do

    it "Spago should list-packages successfully" $ do

      spago ["init"] >>= shouldBeSuccess
      mv "packages.dhall" "packages-old.dhall"
      writeTextFile "packages.dhall" "https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/packages.dhall sha256:9905f07c9c3bd62fb3205e2108515811a89d55cff24f4341652f61ddacfcf148"
      spago ["list-packages"] >>= shouldBeSuccessOutput "list-packages.txt"

    it "Spago should list-packages in JSON successfully" $ do

      spago ["init"] >>= shouldBeSuccess
      mv "packages.dhall" "packages-old.dhall"
      writeTextFile "packages.dhall" "https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/packages.dhall sha256:9905f07c9c3bd62fb3205e2108515811a89d55cff24f4341652f61ddacfcf148"
      spago ["list-packages", "--json"] >>= shouldBeSuccessOutput "list-packages.json"
