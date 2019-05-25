module PscPackageSpec (spec) where

import           Control.Monad.Extra (whenM)
import           Prelude             hiding (FilePath)
import           Test.Hspec          (Spec, afterAll_, around_, beforeAll,
                                      describe, it, shouldBe)
import           Turtle              (FilePath, empty, procStrictWithErr, procs,
                                      rm, testdir, testfile)
import           Utils               (shouldBeFailure, shouldBeSuccess, spago,
                                      withCwd)

testDir :: FilePath
testDir = "test/psc-package-local-test"

clean :: IO ()
clean = withCwd testDir $ do
  let pkgs = "packages.dhall"
  whenM (testfile pkgs) (rm pkgs)
  procs "git" ["checkout", "--", "psc-package.json"] empty

spec :: Spec
spec = beforeAll clean $ afterAll_ clean $ around_ (withCwd testDir) $ do

  describe "spago psc-package-local-setup" $ do

    it "Local setup should succeed on first run" $ do

      spago ["psc-package-local-setup"] >>= shouldBeSuccess
      testfile "packages.dhall" >>= (`shouldBe` True)

    it "Running local setup twice should cause an error with an existing setup" $ do

      spago ["psc-package-local-setup"] >>= shouldBeFailure


  describe "spago psc-package-insdhall" $ do

    it "Insdhall should run successfully" $ do

      spago ["psc-package-insdhall"] >>= shouldBeSuccess
      testfile ".psc-package/local/.set/packages.json" >>= (`shouldBe` True)

    it "Psc-Package build should work successfully" $ do

      procStrictWithErr "psc-package" ["build"] empty >>= shouldBeSuccess


  describe "spago psc-package-clean" $ do

    it "Clean should complete successfully" $ do

      spago ["psc-package-clean"] >>= shouldBeSuccess
      testdir ".psc-package" >>= (`shouldBe` False)
