module Data.Date.Component.Gen where

import Prelude
import Control.Monad.Gen (class MonadGen, chooseInt)
import Data.Date.Component (Day, Month, Weekday, Year)
import Data.Enum (toEnum)
import Data.Enum.Gen (genBoundedEnum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

-- | Generates a random `Year` in the range 1900-2100, inclusive.
genYear :: forall m. MonadGen m => m Year
genYear = unsafePartial fromJust <<< toEnum <$> chooseInt 1900 2100

-- | Generates a random `Month` component.
genMonth :: forall m. MonadGen m => m Month
genMonth = genBoundedEnum

-- | Generates a random `Day` component.
genDay :: forall m. MonadGen m => m Day
genDay = genBoundedEnum

-- | Generates a random `Weekday` component.
genWeekday :: forall m. MonadGen m => m Weekday
genWeekday = genBoundedEnum
