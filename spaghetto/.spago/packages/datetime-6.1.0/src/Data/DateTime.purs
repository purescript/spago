module Data.DateTime
  ( DateTime(..)
  , date
  , modifyDate
  , modifyDateF
  , time
  , modifyTime
  , modifyTimeF
  , adjust
  , diff
  , module Data.Date
  , module Data.Time
  ) where

import Prelude

import Data.Date (Date, Day, Month(..), Weekday(..), Year, canonicalDate, day, exactDate, month, weekday, year)
import Data.Enum (toEnum, fromEnum)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Time (Hour, Millisecond, Minute, Second, Time(..), hour, setHour, millisecond, setMillisecond, minute, setMinute, second, setSecond)
import Data.Time.Duration (class Duration, fromDuration, toDuration, Milliseconds)
import Data.Maybe (Maybe(..))

-- | A date/time value in the Gregorian calendar/UTC time zone.
data DateTime = DateTime Date Time

derive instance eqDateTime :: Eq DateTime
derive instance ordDateTime :: Ord DateTime

instance boundedDateTime :: Bounded DateTime where
  bottom = DateTime bottom bottom
  top = DateTime top top

instance showDateTime :: Show DateTime where
  show (DateTime d t) = "(DateTime " <> show d <> " " <> show t <> ")"

date :: DateTime -> Date
date (DateTime d _) = d

modifyDate :: (Date -> Date) -> DateTime -> DateTime
modifyDate f (DateTime d t) = DateTime (f d) t

modifyDateF :: forall f. Functor f => (Date -> f Date) -> DateTime -> f DateTime
modifyDateF f (DateTime d t) = flip DateTime t <$> f d

time :: DateTime -> Time
time (DateTime _ t) = t

modifyTime :: (Time -> Time) -> DateTime -> DateTime
modifyTime f (DateTime d t) = DateTime d (f t)

modifyTimeF :: forall f. Functor f => (Time -> f Time) -> DateTime -> f DateTime
modifyTimeF f (DateTime d t) = DateTime d <$> f t

-- | Adjusts a date/time value with a duration offset. `Nothing` is returned
-- | if the resulting date would be outside of the range of valid dates.
adjust :: forall d. Duration d => d -> DateTime -> Maybe DateTime
adjust d dt =
  adjustImpl Just Nothing (fromDuration d) (toRecord dt) >>= \rec ->
    DateTime
      <$> join (exactDate <$> toEnum rec.year <*> toEnum rec.month <*> toEnum rec.day)
      <*> (Time <$> toEnum rec.hour <*> toEnum rec.minute <*> toEnum rec.second <*> toEnum rec.millisecond)

-- | Calculates the difference between two date/time values, returning the
-- | result as a duration.
diff :: forall d. Duration d => DateTime -> DateTime -> d
diff dt1 dt2 = toDuration $ runFn2 calcDiff (toRecord dt1) (toRecord dt2)

type DateRec =
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  , second :: Int
  , millisecond :: Int
  }

toRecord :: DateTime -> DateRec
toRecord (DateTime d t) =
  { year: fromEnum (year d)
  , month: fromEnum (month d)
  , day: fromEnum (day d)
  , hour: fromEnum (hour t)
  , minute: fromEnum (minute t)
  , second: fromEnum (second t)
  , millisecond: fromEnum (millisecond t)
  }

-- TODO: these could (and probably should) be implemented in PS

foreign import calcDiff :: Fn2 DateRec DateRec Milliseconds

foreign import adjustImpl
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Milliseconds
  -> DateRec
  -> Maybe DateRec
