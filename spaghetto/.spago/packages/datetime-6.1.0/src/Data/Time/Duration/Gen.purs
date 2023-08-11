module Data.Time.Duration.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Data.Time.Duration (Days(..), Hours(..), Milliseconds(..), Minutes(..), Seconds(..))

-- | Generates a random `Milliseconds` duration, up to 10 minutes.
genMilliseconds :: forall m. MonadGen m => m Milliseconds
genMilliseconds = Milliseconds <$> Gen.chooseFloat 0.0 600000.0

-- | Generates a random `Seconds` duration, up to 10 minutes.
genSeconds :: forall m. MonadGen m => m Seconds
genSeconds = Seconds <$> Gen.chooseFloat 0.0 600.0

-- | Generates a random `Seconds` duration, up to 10 hours.
genMinutes :: forall m. MonadGen m => m Minutes
genMinutes = Minutes <$> Gen.chooseFloat 0.0 600.0

-- | Generates a random `Hours` duration, up to 10 days.
genHours :: forall m. MonadGen m => m Hours
genHours = Hours <$> Gen.chooseFloat 0.0 240.0

-- | Generates a random `Days` duration, up to 6 weeks.
genDays :: forall m. MonadGen m => m Days
genDays = Days <$> Gen.chooseFloat 0.0 42.0
