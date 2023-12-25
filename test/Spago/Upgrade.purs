module Test.Spago.Upgrade where

import Test.Prelude

import Effect.Now as Now
import Spago.Core.Config (SetAddress(..))
import Spago.Core.Config as Core
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
      let initialVersion = mkVersion "20.0.1"
      startingTime <- liftEffect $ Now.now
      maybeConfig <- runSpago { logOptions: { color: false, verbosity: LogQuiet, startingTime } } (Core.readConfig "spago.yaml")
      case maybeConfig of
        Right { yaml: { workspace: Just { packageSet: Just (SetFromRegistry { registry }) } } } | registry > initialVersion -> pure unit
        Right { yaml: c } -> Assert.fail $ "Could not upgrade the package set, config: " <> printJson Core.configCodec c
        Left err -> Assert.fail $ "Could not read config: " <> show err
