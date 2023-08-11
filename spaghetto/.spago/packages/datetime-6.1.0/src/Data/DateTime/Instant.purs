module Data.DateTime.Instant
  ( Instant
  , instant
  , unInstant
  , fromDateTime
  , fromDate
  , toDateTime
  , diff
  ) where

import Prelude

import Data.DateTime (Millisecond, Second, Minute, Hour, Day, Year, DateTime(..), Date, Time(..), canonicalDate, millisecond, second, minute, hour, day, month, year)
import Data.Enum (fromEnum, toEnum)
import Data.Function.Uncurried (Fn7, runFn7)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time.Duration (class Duration, Milliseconds(..), negateDuration, toDuration)
import Partial.Unsafe (unsafePartial)

-- | An instant is a duration in milliseconds relative to the Unix epoch
-- | (1970-01-01 00:00:00 UTC).
-- |
-- | The constructor is private as the `Instant` range matches that of the
-- | `DateTime` type.
newtype Instant = Instant Milliseconds

derive newtype instance eqDateTime :: Eq Instant
derive newtype instance ordDateTime :: Ord Instant

instance boundedInstant :: Bounded Instant where
  bottom = Instant (Milliseconds (-8639977881600000.0))
  top = Instant (Milliseconds 8639977881599999.0)

instance showInstant :: Show Instant where
  show (Instant ms) = "(Instant " <> show ms <> ")"

-- Unfortunately Instant cannot be made a `BoundedEnum` as it "should" be,
-- unless enum cardinality and from/to range is extended to use a numeric type
-- bigger than Int32

-- | Attempts to create an `Instant` from a `Milliseconds` duration. The
-- | minimum acceptable value equates to the `bottom` `DateTime` and the maximum
-- | acceptable value equates to the `top` `DateTime`.
instant :: Milliseconds -> Maybe Instant
instant ms@(Milliseconds n)
  | n >= -8639977881600000.0 && n <= 8639977881599999.0 = Just (Instant ms)
  | otherwise = Nothing

-- | Lowers an `Instant` to a `Milliseconds` duration.
unInstant :: Instant -> Milliseconds
unInstant (Instant ms) = ms

-- | Creates an `Instant` from a `DateTime` value.
fromDateTime :: DateTime -> Instant
fromDateTime (DateTime d t) =
  runFn7 fromDateTimeImpl
    (year d) (fromEnum (month d)) (day d)
    (hour t) (minute t) (second t) (millisecond t)

-- | Creates an `Instant` from a `Date` value, using the assumed time 00:00:00.
fromDate :: Date -> Instant
fromDate d =
  runFn7 fromDateTimeImpl
    (year d) (fromEnum (month d)) (day d)
    bottom bottom bottom bottom

-- | Creates a `DateTime` value from an `Instant`.
toDateTime :: Instant -> DateTime
toDateTime = toDateTimeImpl mkDateTime
  where
  mkDateTime = unsafePartial \y mo d h mi s ms ->
    DateTime (canonicalDate y (fromJust (toEnum mo)) d) (Time h mi s ms)

-- TODO: these could (and probably should) be implemented in PS
foreign import fromDateTimeImpl :: Fn7 Year Int Day Hour Minute Second Millisecond Instant
foreign import toDateTimeImpl :: (Year -> Int -> Day -> Hour -> Minute -> Second -> Millisecond -> DateTime) -> Instant -> DateTime

-- | Calculates the difference between two instants, returning the result as a duration.
-- | For example:
-- | ```
-- | do
-- |   start <- liftEffect Now.now
-- |   aLongRunningAff
-- |   end <- liftEffect Now.now
-- |   let
-- |     hours :: Duration.Hours
-- |     hours = Instant.diff end start
-- |   log ("A long running Aff took " <> show hours)
-- | ```
diff :: forall d. Duration d => Instant → Instant → d
diff dt1 dt2 = toDuration (unInstant dt1 <> negateDuration (unInstant dt2))
