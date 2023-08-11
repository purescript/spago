module Effect.Now
  ( now
  , nowDateTime
  , nowDate
  , nowTime
  , getTimezoneOffset
  ) where

import Prelude

import Data.DateTime (Date, DateTime, Time, date, time)
import Data.Time.Duration (Minutes)
import Data.DateTime.Instant (Instant, toDateTime)
import Effect (Effect)

-- | Gets an `Instant` value for the date and time according to the current
-- | machine’s clock.
foreign import now :: Effect Instant

-- | Gets a `DateTime` value for the date and time according to the current
-- | machine’s clock.
nowDateTime :: Effect DateTime
nowDateTime = toDateTime <$> now

-- | Gets the date according to the current machine’s clock.
nowDate :: Effect Date
nowDate = date <<< toDateTime <$> now

-- | Gets the time according to the current machine’s clock.
nowTime :: Effect Time
nowTime = time <<< toDateTime <$> now

-- | Gets the time zone difference, in minutes, from current local (host system settings) to UTC using `now`.
foreign import getTimezoneOffset :: Effect Minutes
