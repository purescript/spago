module Data.DateTime.Gen
  ( genDateTime
  , module Data.Date.Gen
  , module Data.Time.Gen
  ) where

import Prelude
import Control.Monad.Gen (class MonadGen)
import Data.Date.Gen (genDate, genDay, genMonth, genWeekday, genYear)
import Data.DateTime (DateTime(..))
import Data.Time.Gen (genHour, genMillisecond, genMinute, genSecond, genTime)

-- | Generates a random `DateTime` between 1st Jan 1900 00:00:00 and
-- | 31st Dec 2100 23:59:59, inclusive.
genDateTime :: forall m. MonadGen m => m DateTime
genDateTime = DateTime <$> genDate <*> genTime
