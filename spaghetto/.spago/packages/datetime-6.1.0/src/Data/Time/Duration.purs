module Data.Time.Duration where

import Prelude

import Data.Newtype (class Newtype, over)

-- | A duration measured in milliseconds.
newtype Milliseconds = Milliseconds Number

derive instance newtypeMilliseconds :: Newtype Milliseconds _
derive newtype instance eqMilliseconds :: Eq Milliseconds
derive newtype instance ordMilliseconds :: Ord Milliseconds

instance semigroupMilliseconds :: Semigroup Milliseconds where
  append (Milliseconds x) (Milliseconds y) = Milliseconds (x + y)

instance monoidMilliseconds :: Monoid Milliseconds where
  mempty = Milliseconds 0.0

instance showMilliseconds :: Show Milliseconds where
  show (Milliseconds n) = "(Milliseconds " <> show n <> ")"

-- | A duration measured in seconds.
newtype Seconds = Seconds Number

derive instance newtypeSeconds :: Newtype Seconds _
derive newtype instance eqSeconds :: Eq Seconds
derive newtype instance ordSeconds :: Ord Seconds

instance semigroupSeconds :: Semigroup Seconds where
  append (Seconds x) (Seconds y) = Seconds (x + y)

instance monoidSeconds :: Monoid Seconds where
  mempty = Seconds 0.0

instance showSeconds :: Show Seconds where
  show (Seconds n) = "(Seconds " <> show n <> ")"

-- | A duration measured in minutes.
newtype Minutes = Minutes Number

derive instance newtypeMinutes :: Newtype Minutes _
derive newtype instance eqMinutes :: Eq Minutes
derive newtype instance ordMinutes :: Ord Minutes

instance semigroupMinutes :: Semigroup Minutes where
  append (Minutes x) (Minutes y) = Minutes (x + y)

instance monoidMinutes :: Monoid Minutes where
  mempty = Minutes 0.0

instance showMinutes :: Show Minutes where
  show (Minutes n) = "(Minutes " <> show n <> ")"

-- | A duration measured in hours.
newtype Hours = Hours Number

derive instance newtypeHours :: Newtype Hours _
derive newtype instance eqHours :: Eq Hours
derive newtype instance ordHours :: Ord Hours

instance semigroupHours :: Semigroup Hours where
  append (Hours x) (Hours y) = Hours (x + y)

instance monoidHours :: Monoid Hours where
  mempty = Hours 0.0

instance showHours :: Show Hours where
  show (Hours n) = "(Hours " <> show n <> ")"

-- | A duration measured in days, where a day is assumed to be exactly 24 hours.
newtype Days = Days Number

derive instance newtypeDays :: Newtype Days _
derive newtype instance eqDays :: Eq Days
derive newtype instance ordDays :: Ord Days

instance semigroupDays :: Semigroup Days where
  append (Days x) (Days y) = Days (x + y)

instance monoidDays :: Monoid Days where
  mempty = Days 0.0

instance showDays :: Show Days where
  show (Days n) = "(Days " <> show n <> ")"

-- | A class for enabling conversions between duration types.
class Duration a where
  fromDuration :: a -> Milliseconds
  toDuration :: Milliseconds -> a

-- | Converts directly between durations of differing types.
convertDuration :: forall a b. Duration a => Duration b => a -> b
convertDuration = toDuration <<< fromDuration

-- | Negates a duration, turning a positive duration negative or a negative
-- | duration positive.
negateDuration :: forall a. Duration a => a -> a
negateDuration = toDuration <<< over Milliseconds negate <<< fromDuration

instance durationMilliseconds :: Duration Milliseconds where
  fromDuration = identity
  toDuration = identity

instance durationSeconds :: Duration Seconds where
  fromDuration = over Seconds (_ * 1000.0)
  toDuration = over Milliseconds (_ / 1000.0)

instance durationMinutes :: Duration Minutes where
  fromDuration = over Minutes (_ * 60000.0)
  toDuration = over Milliseconds (_ / 60000.0)

instance durationHours :: Duration Hours where
  fromDuration = over Hours (_ * 3600000.0)
  toDuration = over Milliseconds (_ / 3600000.0)

instance durationDays :: Duration Days where
  fromDuration = over Days (_ * 86400000.0)
  toDuration = over Milliseconds (_ / 86400000.0)
