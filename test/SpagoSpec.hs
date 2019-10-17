module SpagoSpec (spec) where

import           Control.Concurrent (threadDelay)
import qualified Data.Text          as Text
import           Prelude            hiding (FilePath)
import qualified System.IO.Temp     as Temp
import           Test.Hspec         (Spec, around_, describe, it, shouldBe, shouldNotSatisfy,
                                     shouldReturn, shouldSatisfy)
import           Turtle             (ExitCode (..), cd, cp, decodeString, empty, mkdir, mktree, mv,
                                     readTextFile, rm, shell, shellStrictWithErr, testdir,
                                     writeTextFile)
import           Utils              (checkFileHasInfix, checkFixture, readFixture, runFor,
                                     shouldBeFailure, shouldBeFailureOutput, shouldBeSuccess,
                                     shouldBeSuccessOutput, spago, withCwd)


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
      readTextFile "src/Main.purs" `shouldReturn` "Something"

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

    it "Spago should import configs from Bower" $ do

      shellStrictWithErr "git clone https://github.com/justinwoo/purescript-simple-json.git ." empty
      shellStrictWithErr "git checkout v7.0.0" empty
      spago ["init"] >>= shouldBeSuccess
      mv "spago.dhall" "spago-bower-import.dhall"
      checkFixture "spago-bower-import.dhall"

    it "Spago should strip comments from spago.dhall and packages.dhall" $ do

      spago ["init", "--no-comments"] >>= shouldBeSuccess

      cp "spago.dhall" "spago-no-comments.dhall"
      checkFixture "spago-no-comments.dhall"

      -- We don't want fixture for packages.dhall to avoid having to maintain upstream package-set URL in fixture
      dhallSource <- readTextFile "packages.dhall"
      dhallSource `shouldNotSatisfy` (Text.isInfixOf "{-") -- comments not present
      dhallSource `shouldSatisfy` (Text.isInfixOf "let overrides = {=}") -- some dhall stuff is present

  describe "spago install" $ do

    it "Subsequent installs should succeed after failed install" $ do

      spago ["init"] >>= shouldBeSuccess
      -- Run `install` once and kill it soon to simulate failure
      runFor 5000 "spago" ["install", "-j", "3"]
      -- Sleep for some time, as the above might take time to cleanup old processes
      threadDelay 1000000
      spago ["install", "-j", "10"] >>= shouldBeSuccess

    it "Spago should warn that config was not changed, when trying to install package already present in project dependencies" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["install"] >>= shouldBeSuccess
      spago ["install", "effect"] >>= shouldBeSuccessOutput "spago-install-existing-dep-warning.txt"

    it "Spago should strip 'purescript-' prefix and give warning if package without prefix is present in package set" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["install"] >>= shouldBeSuccess
      spago ["install", "purescript-newtype"] >>= shouldBeSuccessOutput "spago-install-purescript-prefix-warning.txt"
      -- dep added without "purescript-" prefix
      checkFileHasInfix "spago.dhall" "\"newtype\""

    it "Spago should be able to add dependencies" $ do

      writeTextFile "psc-package.json" "{ \"name\": \"aaa\", \"depends\": [ \"prelude\" ], \"set\": \"foo\", \"source\": \"bar\" }"
      spago ["init"] >>= shouldBeSuccess
      spago ["-j 10", "install", "simple-json", "foreign"] >>= shouldBeSuccess
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

    it "Spago should be able to update dependencies in an alternative config" $ do

      spago ["init"] >>= shouldBeSuccess
      writeTextFile "alternative1.dhall" "./spago.dhall // {dependencies = [\"prelude\"]}"
      spago ["-x", "alternative1.dhall", "install", "simple-json"] >>= shouldBeSuccess
      checkFixture "alternative1.dhall"

    it "Spago should not change the alternative config if it does not change dependencies" $ do

      spago ["init"] >>= shouldBeSuccess
      writeTextFile "alternative2.dhall" "./spago.dhall // { sources = [ \"src/**/*.purs\" ] }\n"
      spago ["-x", "alternative2.dhall", "install", "simple-json"] >>= shouldBeSuccess
      spago ["-x", "alternative2.dhall", "install", "simple-json"] >>= shouldBeSuccessOutput "alternative2install.txt"
      checkFixture "alternative2.dhall"

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

    it "Spago should not warn about freezing remote imports with the default setup" $ do

      spago ["init"] >>= shouldBeSuccess
      -- Do an initial installation so that it's easier to compare subsequent output
      spago ["install"] >>= shouldBeSuccess
      spago ["install"] >>= shouldBeSuccessOutput "ensure-frozen-success.txt"

    it "Spago should not warn about freezing remote imports if they can be located from spago.dhall" $ do

      spago ["init"] >>= shouldBeSuccess
      -- Do an initial installation so that it's easier to compare subsequent output
      spago ["install"] >>= shouldBeSuccess

      mkdir "elsewhere"
      writeTextFile "elsewhere/a.dhall" "let p = ./b.dhall in p"
      mv "packages.dhall" "elsewhere/b.dhall"
      spagoDhall <- readTextFile "spago.dhall"
      writeTextFile "spago.dhall" $ Text.replace "./packages.dhall" "./elsewhere/a.dhall" spagoDhall
      spago ["install"] >>= shouldBeSuccessOutput "ensure-frozen-success.txt"

  describe "spago sources" $ do

    it "Spago should print both dependencies and project sources" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["sources"] >>= shouldBeSuccessOutput "sources-output.txt"

  -- -- This is currently commented because it requires a GitHub token and Travis makes it hard to do it securely
  -- describe "spago login" $ do
  --
  --  it "Spago should login correctly" $ do
  --
  --    spago ["login"] >>= shouldBeSuccessOutput "login-output.txt"

  describe "spago build" $ do

    it "Spago should build successfully" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["build"] >>= shouldBeSuccess

    it "Spago should pass options to purs" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["build", "--purs-args", "-o myOutput"] >>= shouldBeSuccess
      testdir "myOutput" `shouldReturn` True

    it "Spago should pass multiple options to purs" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["build", "--purs-args", "-o", "--purs-args", "myOutput"] >>= shouldBeSuccess
      testdir "myOutput" `shouldReturn` True

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

    it "Spago should create a local output folder when we are using --no-share-output" $ do

      -- Create root-level packages.dhall
      mkdir "monorepo"
      cd "monorepo"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"

      -- Create local 'lib-a' package that uses packages.dhall on top level (but also has it's own one to confuse things)
      mkdir "lib-a"
      cd "lib-a"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-1\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ./packages.dhall }"
      rm "packages.dhall"
      writeTextFile "packages.dhall" $ "../packages.dhall"
      spago ["build", "--no-share-output"] >>= shouldBeSuccess
      testdir "output" `shouldReturn` True

      cd ".."
      testdir "output" `shouldReturn` False

    it "Spago should use the main packages.dhall even when another packages.dhall is further up the tree" $ do

      -- Create root-level packages.dhall that directs to middle one
      mkdir "monorepo-1"
      cd "monorepo-1"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      rm "packages.dhall"
      writeTextFile "packages.dhall" $ "./monorepo-2/packages.dhall"

      -- Create local 'monorepo-2' package that is the real root
      mkdir "monorepo-2"
      cd "monorepo-2"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-1\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ./packages.dhall }"
      spago ["build", "--no-share-output"] >>= shouldBeSuccess
      testdir "output" `shouldReturn` True

       -- Create local 'monorepo-3' package that uses packages.dhall on top level
      mkdir "monorepo-3"
      cd "monorepo-3"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-1\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ./packages.dhall }"
      rm "packages.dhall"
      writeTextFile "packages.dhall" $ "../../packages.dhall"
      spago ["build"] >>= shouldBeSuccess
      testdir "output" `shouldReturn` False

      cd ".."
      testdir "output" `shouldReturn` True

      cd ".."
      testdir "output" `shouldReturn` False

    it "Spago should find the middle packages.dhall even when another file is further up the tree" $ do

      -- Create root-level module to confuse things
      mkdir "monorepo-root"
      cd "monorepo-root"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-extra\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ./packages.dhall }"

      -- get rid of duplicate Main module
      cd "src"
      rm "Main.purs"
      writeTextFile "Main.purs" $ "module OtherMain where \n import Prelude\n import Effect\n main :: Effect Unit\n main = pure unit"
      cd ".."

      -- create real root
      mkdir "subfolder"
      cd "subfolder"
      spago ["init"] >>= shouldBeSuccess
      packageDhall <- readTextFile "packages.dhall"
      writeTextFile "packages.dhall" $ packageDhall <> " // { lib-extra = ../spago.dhall as Location }"

      -- Create local 'lib-a' package that uses packages.dhall in middle folder
      mkdir "lib-a"
      cd "lib-a"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-1\", dependencies = [\"lib-extra\",\"console\", \"effect\", \"prelude\"], packages = ../packages.dhall }"
      rm "packages.dhall"
      spago ["build"] >>= shouldBeSuccess

      -- don't use nested folder
      testdir "output" `shouldReturn` False

      -- use middle one
      cd ".."
      testdir "output" `shouldReturn` True

      -- not the trick root folder
      cd ".."
      testdir "output" `shouldReturn` False

    it "Spago should create an output folder in the root when we are not passing --no-share-output" $ do

      -- Create root-level packages.dhall
      mkdir "monorepo2"
      cd "monorepo2"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"

      -- Create local 'lib-a' package that uses packages.dhall on top level (but also has it's own one to confuse things)
      mkdir "lib-a"
      cd "lib-a"
      spago ["init"] >>= shouldBeSuccess
      rm "packages.dhall"
      writeTextFile "packages.dhall" $ "../packages.dhall"
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-1\", license = \"MIT\", repository = \"https://github.com/fake/lib-1.git\", dependencies = [\"console\", \"effect\", \"prelude\"], sources = [\"src/**/*.purs\"], packages = ./packages.dhall }"
      spago ["build"] >>= shouldBeSuccess
      testdir "output" `shouldReturn` False

      cd ".."
      testdir "output" `shouldReturn` True

    describe "alternate backend" $ do

      it "Spago should use alternate backend if option is specified" $ do
        configWithBackend <- readFixture "spago-configWithBackend.dhall"
        spago ["init"] >>= shouldBeSuccess
        mv "spago.dhall" "spago-old.dhall"
        writeTextFile "spago.dhall" configWithBackend

        -- Blocked by https://github.com/purescript/purescript/issues/3743
        -- spago ["-j", "5", "build"] >>= shouldBeSuccess
        -- checkFixture "alternate-backend-output.txt"

      it "Passing `--codegen corefn` with backend option should fail" $ do
        configWithBackend <- readFixture "spago-configWithBackend.dhall"
        spago ["init"] >>= shouldBeSuccess
        mv "spago.dhall" "spago-old.dhall"
        writeTextFile "spago.dhall" configWithBackend

        -- Blocked by https://github.com/purescript/purescript/issues/3743
        -- spago ["-j", "5", "build"] >>= shouldBeSuccess
        -- spago ["build", "--purs-args", "--codegen", "--purs-args", "corefn"] >>= shouldBeFailureOutput "codegen-opt-with-backend.txt"
        -- spago ["build", "--purs-args", "--codegen", "--purs-args", "docs"] >>= shouldBeFailureOutput "codegen-opt-with-backend.txt"



  describe "spago test" $ do

    it "Spago should test successfully" $ do

      spago ["init"] >>= shouldBeSuccess
      -- Note: apparently purs starts caching the compiled modules only after three builds
      spago ["build"] >>= shouldBeSuccess
      spago ["build"] >>= shouldBeSuccess
      spago ["test"] >>= shouldBeSuccessOutput "test-output.txt"

    it "Spago should test in custom output folder" $ do

      spago ["init"] >>= shouldBeSuccess
      spago ["test", "--purs-args", "-o", "--purs-args", "myOutput"] >>= shouldBeSuccess
      testdir "myOutput" `shouldReturn` True

    it "Spago should test successfully with a different output folder" $ do

      -- Create root-level packages.dhall
      mkdir "monorepo"
      cd "monorepo"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"

      -- Create local 'lib-a' package that uses packages.dhall on top level (but also has it's own one to confuse things)
      mkdir "lib-a"
      cd "lib-a"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-1\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ./packages.dhall }"
      rm "packages.dhall"
      writeTextFile "packages.dhall" $ "../packages.dhall"
      spago ["test"] >>= shouldBeSuccess
      testdir "output" `shouldReturn` False

      cd ".."
      testdir "output" `shouldReturn` True

    it "Spago should test successfully with --no-share-output" $ do

      -- Create root-level packages.dhall
      mkdir "monorepo"
      cd "monorepo"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"

      -- Create local 'lib-a' package that uses packages.dhall on top level (but also has it's own one to confuse things)
      mkdir "lib-a"
      cd "lib-a"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-1\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ./packages.dhall }"
      rm "packages.dhall"
      writeTextFile "packages.dhall" $ "../packages.dhall"
      spago ["test", "--no-share-output"] >>= shouldBeSuccess
      testdir "output" `shouldReturn` True

      cd ".."
      testdir "output" `shouldReturn` False


  describe "spago upgrade-set" $ do

    it "Spago should migrate package-sets from src/packages.dhall to the released one" $ do

      spago ["init"] >>= shouldBeSuccess
      mv "packages.dhall" "packages-expected.dhall"
      packages <- readTextFile "packages-expected.dhall"
      let [packageSetUrl]
            = map Text.strip
            $ filter (not . null . Text.breakOnAll "https://github.com/purescript/package-sets")
            $ Text.lines packages
      writeTextFile "packages.dhall" "https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191025/packages.dhall sha256:f9eb600e5c2a439c3ac9543b1f36590696342baedab2d54ae0aa03c9447ce7d4"
      spago ["-v", "upgrade-set"] >>= shouldBeSuccess
      newPackages <- Text.strip <$> readTextFile "packages.dhall"
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
      writeTextFile "packages.dhall" "https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191025/packages.dhall sha256:f9eb600e5c2a439c3ac9543b1f36590696342baedab2d54ae0aa03c9447ce7d4"
      spago ["list-packages"] >>= shouldBeSuccessOutput "list-packages.txt"

    it "Spago should list-packages in JSON successfully" $ do

      spago ["init"] >>= shouldBeSuccess
      mv "packages.dhall" "packages-old.dhall"
      writeTextFile "packages.dhall" "https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191025/packages.dhall sha256:f9eb600e5c2a439c3ac9543b1f36590696342baedab2d54ae0aa03c9447ce7d4"
      spago ["list-packages", "--json"] >>= shouldBeSuccessOutput "list-packages.json"

  describe "spago output-path" $ do
    it "Spago should output the correct path" $ do

      -- Create local 'monorepo-1' package that is the real root
      mkdir "monorepo-1"
      cd "monorepo-1"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-1\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ./packages.dhall }"

       -- Create local 'monorepo-2' package that uses packages.dhall on top level
      mkdir "monorepo-2"
      cd "monorepo-2"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-1\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ./packages.dhall }"
      rm "packages.dhall"
      writeTextFile "packages.dhall" $ "../packages.dhall"
      spago ["output-path"] >>= shouldBeSuccessOutput "output-path-remote.txt"

    it "Spago should output the local path when no overrides" $ do

      -- Create local 'monorepo-1' package that is the real root
      mkdir "monorepo-1"
      cd "monorepo-1"
      spago ["init"] >>= shouldBeSuccess
      rm "spago.dhall"
      writeTextFile "spago.dhall" $ "{ name = \"lib-1\", dependencies = [\"console\", \"effect\", \"prelude\"], packages = ./packages.dhall }"
      spago ["output-path", "--no-share-output"] >>= shouldBeSuccessOutput "output-path-local.txt"
