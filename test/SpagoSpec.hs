module SpagoSpec (spec) where

import           Control.Concurrent  (threadDelay)
import           Control.Monad.Extra (whenM)
import           Prelude             hiding (FilePath)
import           Test.Hspec          (Spec, around_, beforeAll, describe, it,
                                      shouldBe)
import           Turtle              (FilePath, cp, mkdir, mv, readTextFile, rm,
                                      rmtree, testdir, writeTextFile)
import           Utils               (checkFixture, runFor, shouldBeFailure,
                                      shouldBeFailureOutput, shouldBeSuccess,
                                      shouldBeSuccessOutput, spago, withCwd)

testDir :: FilePath
testDir = "test/spago-test"

clean :: IO ()
clean = do
  whenM (testdir testDir) (rmtree testDir)
  mkdir testDir

spec :: Spec
spec = beforeAll clean $ around_ (withCwd testDir) $ do

  describe "spago init" $ do

    it "Spago should have set up a project" $ do

      spago ["init"] >>= shouldBeSuccess

    it "Spago should refuse to overwrite an existing project without -f" $ do

      spago ["init"] >>= shouldBeFailure

    it "Cleaning of config files should succeed" $ do

      rm "spago.dhall" :: IO ()
      rm "packages.dhall"

    it "Spago should not overwrite files when initing a project" $ do

      writeTextFile "src/Main.purs" "Something"
      spago ["init"] >>= shouldBeSuccess
      readTextFile "src/Main.purs" >>= (`shouldBe` "Something")
      rmtree "src"

    it "Spago should always succeed in doing init with force" $ do

      spago ["init", "-f"] >>= shouldBeSuccess

    it "Spago should import config from psc-package" $ do

      writeTextFile "psc-package.json" "{ \"name\": \"aaa\", \"depends\": [ \"prelude\" ], \"set\": \"foo\", \"source\": \"bar\" }"
      spago ["init", "-f"] >>= shouldBeSuccess
      cp "spago.dhall" "spago-psc-success.dhall"
      checkFixture "spago-psc-success.dhall"

    it "Spago should not import dependencies that are not in the package-set" $ do

      writeTextFile "psc-package.json" "{ \"name\": \"aaa\", \"depends\": [ \"prelude\", \"foo\", \"bar\" ], \"set\": \"foo\", \"source\": \"bar\" }"
      spago ["init", "-f"] >>= shouldBeSuccess
      cp "spago.dhall" "spago-psc-failure.dhall"
      checkFixture "spago-psc-failure.dhall"


  describe "spago install" $ do

    it "Subsequent installs should succeed after failed install" $ do

      -- Run `install` once and kill it soon to simulate failure
      runFor 5000 "spago" ["install", "-j", "3"]
      -- Sleep for some time, as the above might take time to cleanup old processes
      threadDelay 1000000
      spago ["install", "-j", "10"] >>= shouldBeSuccess

    it "Spago should be able to add dependencies" $ do

      spago ["install", "-j10", "simple-json", "foreign"] >>= shouldBeSuccess
      mv "spago.dhall" "spago-install-success.dhall"
      checkFixture "spago-install-success.dhall"

    it "Spago should always succeed in doing init with force" $ do

      spago ["init", "-f"] >>= shouldBeSuccess

    it "Spago should not add dependencies that are not in the package set" $ do

      spago ["install", "foobar"] >>= shouldBeFailure
      cp "spago.dhall" "spago-install-failure.dhall"
      checkFixture "spago-install-failure.dhall"


  describe "spago build" $ do

    it "Spago should build successfully" $ do

      spago ["build"] >>= shouldBeSuccess

    it "Spago should pass options to purs" $ do

      spago ["build", "--", "-o", "myOutput"] >>= shouldBeSuccess
      testdir "myOutput" >>= (`shouldBe` True)

    it "Spago should build successfully with sources included from custom path" $ do
      mkdir "another_source_path"
      mv "src/Main.purs" "another_source_path/Main.purs"
      spago ["build", "--path", "another_source_path/*.purs"] >>= shouldBeSuccess
      mv "another_source_path/Main.purs" "src/Main.purs"


  describe "spago test" $ do

    it "Spago should test successfully" $ do

      spago ["test"] >>= shouldBeSuccessOutput "test-output.txt"


  describe "spago run" $ do

    it "Spago should run successfully" $ do

      spago ["run", "--verbose"] >>= shouldBeSuccessOutput "run-output.txt"


  describe "spago bundle" $ do

    it "Spago should fail but should point to the replacement command" $ do

      spago ["bundle", "--to", "bundle.js"]
        >>= shouldBeFailureOutput "bundle-output.txt"


  describe "spago bundle-app" $ do

    it "Spago should bundle successfully" $ do
      -- Remove output to ensure bundle builds as well as bundles
      rmtree "output"
      spago ["bundle-app", "--to", "bundle-app.js"] >>= shouldBeSuccess
      checkFixture "bundle-app.js"


  describe "spago make-module" $ do

    it "Spago should fail but should point to the replacement command" $ do

      spago ["make-module", "--to", "make-module.js"]
        >>= shouldBeFailureOutput "make-module-output.txt"


  describe "spago bundle-module" $ do

    it "Spago should successfully make a module" $ do
      -- Now we don't remove the output folder, but we pass the `--no-build`
      -- flag to skip rebuilding (i.e. we are counting on the previous command
      -- to have built stuff for us)
      spago ["bundle-module", "--to", "bundle-module.js", "--no-build"]
        >>= shouldBeSuccess
      checkFixture "bundle-module.js"
