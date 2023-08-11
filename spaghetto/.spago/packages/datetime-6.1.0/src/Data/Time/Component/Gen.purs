module Data.Time.Component.Gen where

import Control.Monad.Gen (class MonadGen)
import Data.Enum.Gen (genBoundedEnum)
import Data.Time.Component (Hour, Millisecond, Minute, Second)

-- | Generates a random `Hour` component.
genHour :: forall m. MonadGen m => m Hour
genHour = genBoundedEnum

-- | Generates a random `Minute` component.
genMinute :: forall m. MonadGen m => m Minute
genMinute = genBoundedEnum

-- | Generates a random `Second` component.
genSecond :: forall m. MonadGen m => m Second
genSecond = genBoundedEnum

-- | Generates a random `Millisecond` component.
genMillisecond :: forall m. MonadGen m => m Millisecond
genMillisecond = genBoundedEnum
