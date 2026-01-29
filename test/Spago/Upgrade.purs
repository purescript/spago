module Test.Spago.Upgrade where

import Test.Prelude

import Data.String as String
import Effect.Now as Now
import Spago.Config (SetAddress(..))
import Spago.Config as Config
import Spago.FS as FS
import Spago.Log (LogVerbosity(..))
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Assertions.String (shouldContain, shouldNotContain)

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "upgrade" do

    Spec.it "can upgrade a package set version" \{ spago, testCwd } -> do
      spago [ "init", "--name", "aaa", "--package-set", "20.0.1" ] >>= shouldBeSuccess
      spago [ "upgrade" ] >>= shouldBeSuccess
      -- we can't just check a fixture here, as there are new package set versions all the time.
      -- so we read the config file, and check that the package set version is more recent than the one we started with
      assertExpectedVersion testCwd
        { check: (_ > mkVersion "20.0.1")
        , error: "Could not upgrade the package set."
        }

    Spec.it "allows to specify package set version" \{ spago, testCwd } -> do
      spago [ "init", "--name", "aaa", "--package-set", "20.0.0" ] >>= shouldBeSuccess
      assertExpectedVersion testCwd
        { check: (_ == mkVersion "20.0.0")
        , error: "Could not init with package set 20.0.0."
        }

      spago [ "upgrade", "--package-set", "20.0.1" ] >>= shouldBeSuccess
      assertExpectedVersion testCwd
        { check: (_ == mkVersion "20.0.1")
        , error: "Could not upgrade the package set to 20.0.1."
        }

    Spec.describe "solver projects" do
      Spec.it "upgrades solver project dependencies" \{ spago, testCwd } -> do
        spago [ "init", "--name", "test-upgrade", "--use-solver" ] >>= shouldBeSuccess
        spago [ "install", "either" ] >>= shouldBeSuccess

        -- Narrow either's range so upgrade will actually change it
        replaceConstraint (testCwd </> "spago.yaml") "either" ">=6.0.0 <6.0.1"

        spago [ "upgrade" ] >>= shouldBeSuccess

        -- Verify range was actually widened (no longer narrow upper bound)
        postConfig <- FS.readTextFile (testCwd </> "spago.yaml")
        postConfig `shouldContain` "either:"
        postConfig `shouldNotContain` "<6.0.1"

        -- Verify project still builds after upgrade
        spago [ "build" ] >>= shouldBeSuccess

      Spec.it "handles empty dependencies gracefully" \{ spago, testCwd, fixture } -> do
        FS.copyTree
          { src: fixture "upgrade/solver-empty-deps"
          , dst: testCwd
          }

        result <- spago [ "upgrade" ]
        shouldBeSuccess result
        either _.stderr _.stderr result `shouldContain` "No dependencies to upgrade"

      Spec.it "upgrades all packages when none selected in multi-package workspace" \{ spago, testCwd, fixture } -> do
        FS.copyTree
          { src: fixture "upgrade/solver-workspace-only"
          , dst: testCwd
          }

        -- Save original configs for comparison
        subpkgOriginal <- FS.readTextFile (testCwd </> "subpkg" </> "spago.yaml")
        subpkg2Original <- FS.readTextFile (testCwd </> "subpkg2" </> "spago.yaml")

        -- With -p flag should upgrade only selected package
        spago [ "upgrade", "-p", "subpkg" ] >>= shouldBeSuccess

        -- Verify subpkg was upgraded (different from original)
        subpkgAfterP <- FS.readTextFile (testCwd </> "subpkg" </> "spago.yaml")
        subpkgAfterP `Assert.shouldNotEqual` subpkgOriginal

        -- Verify subpkg2 was NOT upgraded (still matches original)
        subpkg2AfterP <- FS.readTextFile (testCwd </> "subpkg2" </> "spago.yaml")
        subpkg2AfterP `Assert.shouldEqual` subpkg2Original

        -- Without -p flag, should upgrade all packages
        spago [ "upgrade" ] >>= shouldBeSuccess

        -- Now verify subpkg2 was upgraded (different from original)
        subpkg2AfterAll <- FS.readTextFile (testCwd </> "subpkg2" </> "spago.yaml")
        subpkg2AfterAll `Assert.shouldNotEqual` subpkg2Original

      Spec.it "upgrades test dependencies" \{ spago, testCwd } -> do
        spago [ "init", "--name", "test-deps", "--use-solver" ] >>= shouldBeSuccess
        spago [ "install", "--test-deps", "assert" ] >>= shouldBeSuccess

        spago [ "upgrade" ] >>= shouldBeSuccess

        -- Verify test deps still exist and bare deps stay bare (no colon = no range)
        postConfig <- FS.readTextFile (testCwd </> "spago.yaml")
        postConfig `shouldContain` "- assert"
        postConfig `shouldNotContain` "assert:"
        postConfig `shouldContain` "test:"

        -- Verify project still builds after upgrade
        spago [ "build" ] >>= shouldBeSuccess

      Spec.it "unions existing ranges with new caret range" \{ spago, testCwd } -> do
        spago [ "init", "--name", "test-union", "--use-solver" ] >>= shouldBeSuccess

        -- Set a narrow range: wide lower bound (>=5.0.0) and narrow upper bound (<6.0.1)
        -- This tests both aspects of union:
        -- - Lower bound should be preserved (not bumped to latest)
        -- - Upper bound should be extended (via union with caret of latest)
        replaceConstraint (testCwd </> "spago.yaml") "prelude" ">=5.0.0 <6.0.1"

        spago [ "upgrade" ] >>= shouldBeSuccess

        postConfig <- FS.readTextFile (testCwd </> "spago.yaml")
        -- Lower bound preserved (not bumped to >=6.x.x)
        postConfig `shouldContain` ">=5.0.0"
        -- Upper bound extended (no longer the narrow <6.0.1)
        postConfig `shouldNotContain` "<6.0.1"

      Spec.it "keeps exact versions as exact versions, and * as they are" \{ spago, testCwd } -> do
        spago [ "init", "--name", "test-exact", "--use-solver" ] >>= shouldBeSuccess

        -- Set an exact version constraint
        replaceConstraint (testCwd </> "spago.yaml") "prelude" "6.0.0"

        spago [ "upgrade" ] >>= shouldBeSuccess

        postConfig <- FS.readTextFile (testCwd </> "spago.yaml")
        -- Should still be an exact version (no >= or <), just a different version
        postConfig `shouldNotContain` ">="
        postConfig `shouldNotContain` "<"
        -- Should have prelude with a colon (not bare), indicating it has a constraint
        postConfig `shouldContain` "prelude:"
        -- Original version should be replaced
        postConfig `shouldNotContain` "6.0.0"

        -- Set widest range (*)
        replaceConstraint (testCwd </> "spago.yaml") "prelude" "*"

        spago [ "upgrade" ] >>= shouldBeSuccess

        postConfig' <- FS.readTextFile (testCwd </> "spago.yaml")
        -- Should still be "*"
        postConfig' `shouldContain` "prelude: \"*\""

  where
  -- Helper to replace a dependency constraint in spago.yaml
  replaceConstraint :: LocalPath -> String -> String -> Aff Unit
  replaceConstraint configPath pkgName newConstraint = do
    config <- FS.readTextFile configPath
    let
      lines = String.split (String.Pattern "\n") config
      updatedLines = map updateLine lines
      pattern = "- " <> pkgName
      updateLine line
        | String.contains (String.Pattern pattern) line = "    - " <> pkgName <> ": \"" <> newConstraint <> "\""
        | otherwise = line
    FS.writeTextFile configPath (String.joinWith "\n" updatedLines)

  assertExpectedVersion root { check, error } = do
    startingTime <- liftEffect $ Now.now
    maybeConfig <- runSpago
      { logOptions: { color: false, verbosity: LogQuiet, startingTime }, rootPath: root }
      (Config.readConfig $ root </> "spago.yaml")
    case maybeConfig of
      Right { yaml: { workspace: Just { packageSet: Just (SetFromRegistry { registry }) } } } | check registry -> pure unit
      Right { yaml: c } -> Assert.fail $ error <> " Config: " <> printJson Config.configCodec c
      Left err -> Assert.fail $ "Could not read config: " <> show err
