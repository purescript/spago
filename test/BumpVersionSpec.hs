module BumpVersionSpec (spec) where

import           Prelude        hiding (FilePath)
import qualified System.IO.Temp as Temp
import           Test.Hspec     (Spec, around_, before_, describe, it, shouldBe)
import           Turtle         (Text, cptree, decodeString, mv)
import           Utils          (checkFixture, getHighestTag, git,
                                 shouldBeEmptySuccess, shouldBeFailure,
                                 shouldBeSuccess, spago, withCwd)


setup :: IO () -> IO ()
setup cmd = do
  Temp.withTempDirectory "test/" "bump-version-test" $ \temp -> do
    let tempPath = decodeString temp
    cptree "test/bump-version-test" tempPath
    withCwd tempPath cmd

initGit :: IO ()
initGit = do
  git ["init"] >>= shouldBeSuccess
  commitAll

commitAll :: IO ()
commitAll = do
  git ["add", "--all"] >>= shouldBeSuccess
  git ["commit", "--message", "Initial commit"] >>= shouldBeSuccess

initGitTag :: Text -> IO ()
initGitTag tag = do
  initGit
  git ["tag", "--annotate", tag, "--message", ""] >>= shouldBeSuccess

spec :: Spec
spec = around_ setup $ do

  describe "spago bump-version" $ do

    it "Spago should complain when no git repo exists" $ do

      spago ["bump-version", "minor"] >>= shouldBeFailure

    before_ initGit $ it "Spago should use v0.0.0 as initial version" $ do

      spago ["bump-version", "patch"] >>= shouldBeSuccess
      getHighestTag >>= (`shouldBe` Just "v0.0.1")

    before_ (initGitTag "v1.3.4") $ it "Spago should bump patch version" $ do

      spago ["bump-version", "patch"] >>= shouldBeSuccess
      getHighestTag >>= (`shouldBe` Just "v1.3.5")

    before_ (initGitTag "v1.3.4") $ it "Spago should bump minor version" $ do

      spago ["bump-version", "minor"] >>= shouldBeSuccess
      getHighestTag >>= (`shouldBe` Just "v1.4.0")

    before_ (initGitTag "v1.3.4") $ it "Spago should bump major version" $ do

      spago ["bump-version", "major"] >>= shouldBeSuccess
      getHighestTag >>= (`shouldBe` Just "v2.0.0")

    before_ initGit $ it "Spago should set exact version" $ do

      spago ["bump-version", "v3.1.5"] >>= shouldBeSuccess
      getHighestTag >>= (`shouldBe` Just "v3.1.5")

    before_ initGit $ it "Spago should create bower.json" $ do

      spago ["bump-version", "minor"] >>= shouldBeSuccess
      git ["status", "--porcelain"] >>= shouldBeEmptySuccess
      mv "bower.json" "bump-version-bower.json"
      checkFixture "bump-version-bower.json"

    before_ initGit $ it "Spago should fail when bower.json is not tracked" $ do

      appendFile ".gitignore" "bower.json\n"
      commitAll
      spago ["bump-version", "minor"] >>= shouldBeFailure
