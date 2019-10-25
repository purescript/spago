module BumpVersionSpec (spec) where

import           Data.Versions         (SemVer (..), VUnit (..))
import           Prelude               hiding (FilePath)
import qualified System.IO.Temp        as Temp
import           Test.Hspec            (Spec, around_, before_, describe, it, shouldBe,
                                        shouldReturn)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Gen, arbitrary, forAll)
import           Turtle                (Text, cp, decodeString, mkdir, mv, writeTextFile)
import           Utils                 (checkFileHasInfix, checkFixture, getHighestTag, git,
                                        shouldBeFailure, shouldBeFailureInfix, shouldBeSuccess,
                                        spago, withCwd)

import           Spago.Version         (VersionBump (..), getNextVersion, parseVersion,
                                        parseVersionBump, unparseVersion)

-- fix the package set so bower.json is generated with predictable versions
packageSet :: Text
packageSet =
  "let upstream = https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191025/packages.dhall \
  \ sha256:f9eb600e5c2a439c3ac9543b1f36590696342baedab2d54ae0aa03c9447ce7d4 \
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

randomSemVer :: Gen SemVer
randomSemVer = SemVer <$> arbitrary <*> arbitrary <*> arbitrary <*> pure [] <*> pure []

spec :: Spec
spec = describe "spago bump-version" $ do
  describe "property tests" $ do

    describe "getNextVersion" $ do

      prop "Major version bump should +1 major version and set minor and patch to 0" $
        forAll randomSemVer $ \initialVersion ->
            let Right newVersion = getNextVersion Major initialVersion
            in   _svMajor newVersion == _svMajor initialVersion + 1
              && _svMinor newVersion == 0
              && _svPatch newVersion == 0

      prop "Minor version bump should keep major, +1 minor and set patch to 0" $
        forAll randomSemVer $ \initialVersion ->
            let Right newVersion = getNextVersion Minor initialVersion
            in   _svMajor newVersion == _svMajor initialVersion
              && _svMinor newVersion == _svMinor initialVersion + 1
              && _svPatch newVersion == 0

      prop "Patch version bump should keep major and minor and +1 patch" $
        forAll randomSemVer $ \initialVersion ->
            let Right newVersion = getNextVersion Patch initialVersion
            in   _svMajor newVersion == _svMajor initialVersion
              && _svMinor newVersion == _svMinor initialVersion
              && _svPatch newVersion == _svPatch initialVersion + 1

      prop "parseVersion . unparseVersion == id" $
        forAll randomSemVer $ \v -> parseVersion (unparseVersion v) == Right v

  describe "unit tests" $ do

    describe "parseVersionBump" $ do

      it "should parse 'major'" $
        parseVersionBump "major" `shouldBe` Just Major

      it "should parse 'minor'" $
        parseVersionBump "minor" `shouldBe` Just Minor

      it "should parse 'patch'" $
        parseVersionBump "patch" `shouldBe` Just Patch

      it "should parse version starting with 'v'" $
        parseVersionBump "v1.2.3" `shouldBe` Just (Exact (SemVer 1 2 3 [] []))

      it "should parse version not starting with 'v'" $
        parseVersionBump "1.2.3" `shouldBe` Just (Exact (SemVer 1 2 3 [] []))

      -- TODO is this desired behavior, or should we just drop ONE 'v'? I'd agree it's edge case, but still :-)
      it "should drop multiple 'v's from the beginning" $
        parseVersionBump "vvvvvvvv1.2.3" `shouldBe` Just (Exact (SemVer 1 2 3 [] []))

      -- TODO should this work or should we strip these in parser implementation?
      it "should parse versions with PREREL and META tags" $
        parseVersionBump "1.2.3-r1+git123" `shouldBe` Just (Exact (SemVer 1 2 3 [[Str "r",Digits 1]] [[Str "git", Digits 123]]))

      it "should not parse version which is not semantic" $ do
        parseVersionBump "" `shouldBe` Nothing
        parseVersionBump "1" `shouldBe` Nothing
        parseVersionBump "1.2" `shouldBe` Nothing
        parseVersionBump "1.2.3.4" `shouldBe` Nothing

  around_ setup $ do
    describe "end to end tests" $ do

      it "Spago should complain when no git repo exists" $ do

        spago ["bump-version", "minor"] >>= shouldBeFailureInfix
          "Your git working tree is dirty. Please commit or stash your changes first"

      before_ (initGitTag "v1.2.3") $ it "Spago should only make a tag with `--no-dry-run`" $ do

        spago ["bump-version", "minor"] >>= shouldBeSuccess
        getHighestTag `shouldReturn` Just "v1.2.3"

        spago ["bump-version", "--no-dry-run", "minor"] >>= shouldBeSuccess
        getHighestTag `shouldReturn` Just "v1.3.0"

      before_ (initGitTag "not-a-version") $ it "Spago should use v0.0.0 as initial version" $ do

        spago ["bump-version", "--no-dry-run", "patch"] >>= shouldBeSuccess
        getHighestTag `shouldReturn` Just "v0.0.1"


      before_ initGit $ it "Spago should create bower.json, but not commit it" $ do

        spago ["bump-version", "--no-dry-run", "minor"] >>= shouldBeFailureInfix
           "A new bower.json has been generated. Please commit this and run `bump-version` again."
        mv "bower.json" "bump-version-bower.json"
        checkFixture "bump-version-bower.json"

      before_ initGit $ it "Spago should fail when bower.json is not tracked" $ do

        appendFile ".gitignore" "bower.json\n"
        commitAll
        spago ["bump-version", "minor"] >>= shouldBeFailureInfix
          "bower.json is being ignored by git - change this before continuing"

      before_ initGit $ it "Spago should generate URL#version for non-tagged dependency" $ do

        setOverrides "{ tortellini = upstream.tortellini // { version = \"master\" } }"
        spago ["bump-version", "--no-dry-run", "minor"] >>= shouldBeFailure
        checkFileHasInfix "bower.json" "\"purescript-tortellini\": \"https://github.com/justinwoo/purescript-tortellini.git#master\""

      before_ initGit $ it "Spago should fail when spago.dhall references local dependency" $ do

        mkdir "purescript-tortellini"
        withCwd "purescript-tortellini" $ spago ["init"] >>= shouldBeSuccess
        setOverrides "{ tortellini = ./purescript-tortellini/spago.dhall as Location }"
        spago ["bump-version", "minor"] >>= shouldBeFailureInfix
          "Unable to create Bower version for local repo: ./purescript-tortellini"
