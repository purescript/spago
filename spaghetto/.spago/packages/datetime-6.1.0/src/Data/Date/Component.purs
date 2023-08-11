module Data.Date.Component
  ( Year
  , Month(..)
  , Day
  , Weekday(..)
  ) where

import Prelude

import Data.Enum (class Enum, class BoundedEnum, toEnum, fromEnum, Cardinality(..))
import Data.Maybe (Maybe(..))

-- | A year component for a date.
-- |
-- | The constructor is private as the `Year` type is bounded to the range
-- | -271820 to 275759, inclusive. The `toEnum` function can be used to safely
-- | acquire a year value from an integer.
newtype Year = Year Int

derive newtype instance eqYear :: Eq Year
derive newtype instance ordYear :: Ord Year

-- Note: these seemingly arbitrary bounds come from relying on JS for date
-- manipulations, as it only supports date ±100,000,000 days of the Unix epoch.
-- Using these year values means `Date bottom bottom bottom` is a valid date,
-- likewise for `top`.
instance boundedYear :: Bounded Year where
  bottom = Year (-271820)
  top = Year 275759

instance enumYear :: Enum Year where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumYear :: BoundedEnum Year where
  cardinality = Cardinality 547580
  toEnum n
    | n >= (-271820) && n <= 275759 = Just (Year n)
    | otherwise = Nothing
  fromEnum (Year n) = n

instance showYear :: Show Year where
  show (Year y) = "(Year " <> show y <> ")"

-- | A month component for a date in the Gregorian calendar.
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

derive instance eqMonth :: Eq Month
derive instance ordMonth :: Ord Month

instance boundedMonth :: Bounded Month where
  bottom = January
  top = December

instance enumMonth :: Enum Month where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumMonth :: BoundedEnum Month where
  cardinality = Cardinality 12
  toEnum = case _ of
    1 -> Just January
    2 -> Just February
    3 -> Just March
    4 -> Just April
    5 -> Just May
    6 -> Just June
    7 -> Just July
    8 -> Just August
    9 -> Just September
    10 -> Just October
    11 -> Just November
    12 -> Just December
    _ -> Nothing
  fromEnum = case _ of
    January -> 1
    February -> 2
    March -> 3
    April -> 4
    May -> 5
    June -> 6
    July -> 7
    August -> 8
    September -> 9
    October -> 10
    November -> 11
    December -> 12

instance showMonth :: Show Month where
  show January = "January"
  show February = "February"
  show March = "March"
  show April = "April"
  show May = "May"
  show June = "June"
  show July = "July"
  show August = "August"
  show September = "September"
  show October = "October"
  show November = "November"
  show December = "December"

-- | A day component for a date.
-- |
-- | The constructor is private as the `Day` type is bounded to the range
-- | 1 to 31, inclusive. The `toEnum` function can be used to safely
-- | acquire a day value from an integer.
newtype Day = Day Int

derive newtype instance eqDay :: Eq Day
derive newtype instance ordDay :: Ord Day

instance boundedDay :: Bounded Day where
  bottom = Day 1
  top = Day 31

instance enumDay :: Enum Day where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumDay :: BoundedEnum Day where
  cardinality = Cardinality 31
  toEnum n
    | n >= 1 && n <= 31 = Just (Day n)
    | otherwise = Nothing
  fromEnum (Day n) = n

instance showDay :: Show Day where
  show (Day d) = "(Day " <> show d <> ")"

-- | A type representing the days of the week in the Gregorian calendar.
data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

derive instance eqWeekday :: Eq Weekday
derive instance ordWeekday :: Ord Weekday

instance boundedWeekday :: Bounded Weekday where
  bottom = Monday
  top = Sunday

instance enumWeekday :: Enum Weekday where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumWeekday :: BoundedEnum Weekday where
  cardinality = Cardinality 7
  toEnum = case _ of
    1 -> Just Monday
    2 -> Just Tuesday
    3 -> Just Wednesday
    4 -> Just Thursday
    5 -> Just Friday
    6 -> Just Saturday
    7 -> Just Sunday
    _ -> Nothing
  fromEnum = case _ of
    Monday -> 1
    Tuesday -> 2
    Wednesday -> 3
    Thursday -> 4
    Friday -> 5
    Saturday -> 6
    Sunday -> 7

instance showWeekday :: Show Weekday where
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"
  show Saturday = "Saturday"
  show Sunday = "Sunday"
