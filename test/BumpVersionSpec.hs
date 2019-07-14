module BumpVersionSpec (spec) where

import           Prelude        hiding (FilePath)
import qualified System.IO.Temp as Temp
import           Test.Hspec     (Spec, around_, before_, describe, it, shouldBe)
import           Turtle         (Text, cp, decodeString, mkdir, mv,
                                 writeTextFile)
import           Utils          (checkFixture, getHighestTag, git,
                                 shouldBeFailureInfix, shouldBeSuccess,
                                 shouldBeSuccessInfix, spago, withCwd)


-- fix the package set so bower.json is generated with predictable versions
packageSet :: Text
packageSet =
  "let upstream = https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190614/src/packages.dhall \
  \ sha256:5cbf2418298e7de762401c5719c6eb18eda4c67ba512b3f076b50a793a7fc482 \
  \ in upstream"

setup :: IO () -> IO ()
setup cmd = do
  Temp.withTempDirectory "test/" "bump-version-test" $ \temp -> do
    withCwd (decodeString temp) (setupSpago *> cmd)
  where
    setupSpago = do
      spago ["init"] >>= shouldBeSuccess
      appendFile "spago.dhall" " // { license = \"MIT\", repository = \"git://github.com/spago/not-a-real-repo.git\" }"
      writeTextFile "packages.dhall" packageSet
      spago ["install", "tortellini"]

initGit :: IO ()
initGit = do
  git ["init"] >>= shouldBeSuccess
  git ["config", "user.email", "spago@example.com"] >>= shouldBeSuccess
  git ["config", "user.name", "Giovanni Spago"] >>= shouldBeSuccess
  commitAll

commitAll :: IO ()
commitAll = do
  git ["add", "--all"] >>= shouldBeSuccess
  git ["commit", "--allow-empty-message", "--message", ""] >>= shouldBeSuccess

initGitTag :: Text -> IO ()
initGitTag tag = do
  initGit
  git ["tag", "--annotate", tag, "--message", ""] >>= shouldBeSuccess
  -- commit the bower.json, so spago doesn't fail when it
  -- generates it but then can't commit it automatically
  cp "../fixtures/bump-version-bower.json" "bower.json"
  commitAll

setOverrides :: Text -> IO ()
setOverrides overrides = do
  writeTextFile "packages.dhall" $ packageSet <> " // " <> overrides
  commitAll

spec :: Spec
spec = around_ setup $ do

  describe "spago bump-version" $ do

    it "Spago should complain when no git repo exists" $ do

      spago ["bump-version", "minor"] >>= shouldBeFailureInfix
        "Your git working tree is dirty. Please commit or stash your changes first"

    before_ (initGitTag "v1.2.3") $ it "Spago should not make a tag when not passing --no-dry-run" $ do

      spago ["bump-version", "minor"] >>= shouldBeSuccessInfix
        "Skipped creating new Git tag (v1.3.0) because this is a dry run."
      getHighestTag >>= (`shouldBe` Just "v1.2.3")

    before_ (initGitTag "not-a-version") $ it "Spago should use v0.0.0 as initial version" $ do

      spago ["bump-version", "--no-dry-run", "patch"] >>= shouldBeSuccess
      getHighestTag >>= (`shouldBe` Just "v0.0.1")

    before_ (initGitTag "v1.3.4") $ it "Spago should bump patch version" $ do

      spago ["bump-version", "--no-dry-run", "patch"] >>= shouldBeSuccess
      getHighestTag >>= (`shouldBe` Just "v1.3.5")

    before_ (initGitTag "v1.3.4") $ it "Spago should bump minor version" $ do

      spago ["bump-version", "--no-dry-run", "minor"] >>= shouldBeSuccess
      getHighestTag >>= (`shouldBe` Just "v1.4.0")

    before_ (initGitTag "v1.3.4") $ it "Spago should bump major version" $ do

      spago ["bump-version", "--no-dry-run", "major"] >>= shouldBeSuccess
      getHighestTag >>= (`shouldBe` Just "v2.0.0")

    before_ (initGitTag "v0.0.1") $ it "Spago should set exact version" $ do

      spago ["bump-version", "--no-dry-run", "v3.1.5"] >>= shouldBeSuccess
      getHighestTag >>= (`shouldBe` Just "v3.1.5")

    before_ initGit $ it "Spago should create bower.json, but not commit it" $ do

      spago ["bump-version", "--no-dry-run", "minor"] >>= shouldBeFailureInfix
         "A new bower.json has been generated. Please commit this and run `bump-version` again."
      mv "bower.json" "bump-version-bower.json"
      checkFixture "bump-version-bower.json"

    before_ initGit $ it "Spago should fail when bower.json is not tracked" $ do

      appendFile ".gitignore" "bower.json\n"
      commitAll
      spago ["bump-version", "minor"] >>= shouldBeFailureInfix
        "bower.json is being ignored by git - change this before continuing."

    before_ initGit $ it "Spago should fail when spago.dhall references non-tagged dependency" $ do

      setOverrides "{ tortellini = upstream.tortellini // { version = \"master\" } }"
      spago ["bump-version", "minor"] >>= shouldBeFailureInfix
        "Unable to create Bower version from non-tag version: tortellini master"

    before_ initGit $ it "Spago should fail when spago.dhall references local dependency" $ do

      mkdir "purescript-tortellini"
      withCwd "purescript-tortellini" $ spago ["init"] >>= shouldBeSuccess
      setOverrides "{ tortellini = upstream.tortellini // { repo = \"./purescript-tortellini\" } }"
      spago ["bump-version", "minor"] >>= shouldBeFailureInfix
        "Unable to create Bower version for local repo: ./purescript-tortellini"
