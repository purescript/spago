module Test.Spago.Upgrade where

import Test.Prelude

import Effect.Now as Now
import Spago.Config (SetAddress(..))
import Spago.Config as Config
import Spago.Log (LogVerbosity(..))
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "upgrade" do

    Spec.it "can upgrade a package set version" \{ spago } -> do
      spago [ "init", "--name", "aaa", "--package-set", "20.0.1" ] >>= shouldBeSuccess
      spago [ "upgrade" ] >>= shouldBeSuccess
      -- we can't just check a fixture here, as there are new package set versions all the time.
      -- so we read the config file, and check that the package set version is more recent than the one we started with
      assertExpectedVersion
        { check: (_ > mkVersion "20.0.1")
        , error: "Could not upgrade the package set."
        }

    Spec.it "allows to specify package set version" \{ spago } -> do
      spago [ "init", "--name", "aaa", "--package-set", "20.0.0" ] >>= shouldBeSuccess
      assertExpectedVersion
        { check: (_ == mkVersion "20.0.0")
        , error: "Could not init with package set 20.0.0."
        }

      spago [ "upgrade", "--package-set", "20.0.1" ] >>= shouldBeSuccess
      assertExpectedVersion
        { check: (_ == mkVersion "20.0.1")
        , error: "Could not upgrade the package set to 20.0.1."
        }

    where
      assertExpectedVersion { check, error } = do
        startingTime <- liftEffect $ Now.now
        maybeConfig <- runSpago { logOptions: { color: false, verbosity: LogQuiet, startingTime } } (Config.readConfig "spago.yaml")
        case maybeConfig of
          Right { yaml: { workspace: Just { packageSet: Just (SetFromRegistry { registry }) } } } | check registry -> pure unit
          Right { yaml: c } -> Assert.fail $ error <> " Config: " <> printJson Config.configCodec c
          Left err -> Assert.fail $ "Could not read config: " <> show err
