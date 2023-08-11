module Data.Time
  ( Time(..)
  , hour, setHour
  , minute, setMinute
  , second, setSecond
  , millisecond, setMillisecond
  , adjust
  , diff
  , module Data.Time.Component
  ) where

import Prelude

import Data.Enum (fromEnum, toEnum)
import Data.Int as Int
import Data.Maybe (fromJust)
import Data.Number as Number
import Data.Newtype (unwrap)
import Data.Time.Component (Hour, Millisecond, Minute, Second)
import Data.Time.Duration (class Duration, Days(..), Milliseconds(..), fromDuration, negateDuration, toDuration)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

data Time = Time Hour Minute Second Millisecond

derive instance eqTime :: Eq Time
derive instance ordTime :: Ord Time

instance boundedTime :: Bounded Time where
  bottom = Time bottom bottom bottom bottom
  top = Time top top top top

instance showTime :: Show Time where
  show (Time h m s ms) = "(Time " <> show h <> " " <> show m <> " " <> show s <> " " <> show ms <> ")"

-- | The hour component of a time value.
hour :: Time -> Hour
hour (Time h _ _ _) = h

-- | Alters the hour component of a time value.
setHour :: Hour -> Time -> Time
setHour h (Time _ m s ms) = Time h m s ms

-- | The minute component of a time value.
minute :: Time -> Minute
minute (Time _ m _ _) = m

-- | Alters the minute component of a time value.
setMinute :: Minute -> Time -> Time
setMinute m (Time h _ s ms) = Time h m s ms

-- | The second component of a time value.
second :: Time -> Second
second (Time _ _ s _) = s

-- | Alters the second component of a time value.
setSecond :: Second -> Time -> Time
setSecond s (Time h m _ ms) = Time h m s ms

-- | The millisecond component of a time value.
millisecond :: Time -> Millisecond
millisecond (Time _ _ _ ms) = ms

-- | Alters the millisecond component of a time value.
setMillisecond :: Millisecond -> Time -> Time
setMillisecond ms (Time h m s _) = Time h m s ms

-- | Adjusts a time value with a duration offset. The result includes a
-- | remainder value of the whole number of days involved in the adjustment,
-- | for example, if a time of 23:00:00:00 has a duration of +2 hours added to
-- | it, the result will be 1 day, and 01:00:00:00. Correspondingly, if the
-- | duration is negative, a negative number of days may also be returned as
-- | the remainder.
adjust :: forall d. Duration d => d -> Time -> Tuple Days Time
adjust d t =
  let
    d' = fromDuration d
    tLength = timeToMillis t
    dayLength = 86400000.0
    wholeDays = Days $ Number.floor (unwrap d' / dayLength)
    msAdjust = d' <> negateDuration (fromDuration wholeDays)
    msAdjusted = tLength <> msAdjust
    wrap = if msAdjusted > maxTime then 1.0 else if msAdjusted < minTime then -1.0 else 0.0
  in
    Tuple
      (wholeDays <> Days wrap)
      (millisToTime (msAdjusted <> Milliseconds (dayLength * -wrap)))

maxTime :: Milliseconds
maxTime = timeToMillis top

minTime :: Milliseconds
minTime = timeToMillis bottom

timeToMillis :: Time -> Milliseconds
timeToMillis t = Milliseconds
  $ 3600000.0 * Int.toNumber (fromEnum (hour t))
  + 60000.0 * Int.toNumber (fromEnum (minute t))
  + 1000.0 * Int.toNumber (fromEnum (second t))
  + Int.toNumber (fromEnum (millisecond t))

millisToTime :: Milliseconds -> Time
millisToTime (Milliseconds ms') =
  let
    hourLength = 3600000.0
    minuteLength = 60000.0
    secondLength = 1000.0
    hours = Number.floor (ms' / hourLength)
    minutes = Number.floor ((ms' - hours * hourLength) / minuteLength)
    seconds = Number.floor ((ms' - (hours * hourLength + minutes * minuteLength)) / secondLength)
    milliseconds = ms' - (hours * hourLength + minutes * minuteLength + seconds * secondLength)
  in
    unsafePartial fromJust $
      Time
        <$> toEnum (Int.floor hours)
        <*> toEnum (Int.floor minutes)
        <*> toEnum (Int.floor seconds)
        <*> toEnum (Int.floor milliseconds)

-- | Calculates the difference between two times, returning the result as a
-- | duration.
diff :: forall d. Duration d => Time -> Time -> d
diff t1 t2 = toDuration (timeToMillis t1 <> negateDuration (timeToMillis t2))
