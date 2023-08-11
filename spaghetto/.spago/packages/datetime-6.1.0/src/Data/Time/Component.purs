module Data.Time.Component
  ( Hour
  , Minute
  , Second
  , Millisecond
  ) where

import Prelude

import Data.Enum (class Enum, class BoundedEnum, toEnum, fromEnum, Cardinality(..))
import Data.Maybe (Maybe(..))

-- | An hour component for a time value.
-- |
-- | The constructor is private as values for the type are restricted to the
-- | range 0 to 23, inclusive. The `toEnum` function can be used to safely
-- | acquire an `Hour` value from an integer. Correspondingly, an `Hour` can be
-- | lowered to a plain integer with the `fromEnum` function.
newtype Hour = Hour Int

derive newtype instance eqHour :: Eq Hour
derive newtype instance ordHour :: Ord Hour

instance boundedHour :: Bounded Hour where
  bottom = Hour 0
  top = Hour 23

instance enumHour :: Enum Hour where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumHour :: BoundedEnum Hour where
  cardinality = Cardinality 24
  toEnum n
    | n >= 0 && n <= 23 = Just (Hour n)
    | otherwise = Nothing
  fromEnum (Hour n) = n

instance showHour :: Show Hour where
  show (Hour h) = "(Hour " <> show h <> ")"

-- | An minute component for a time value.
-- |
-- | The constructor is private as values for the type are restricted to the
-- | range 0 to 59, inclusive. The `toEnum` function can be used to safely
-- | acquire an `Minute` value from an integer. Correspondingly, a `Minute` can
-- | be lowered to a plain integer with the `fromEnum` function.
newtype Minute = Minute Int

derive newtype instance eqMinute :: Eq Minute
derive newtype instance ordMinute :: Ord Minute

instance boundedMinute :: Bounded Minute where
  bottom = Minute 0
  top = Minute 59

instance enumMinute :: Enum Minute where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumMinute :: BoundedEnum Minute where
  cardinality = Cardinality 60
  toEnum n
    | n >= 0 && n <= 59 = Just (Minute n)
    | otherwise = Nothing
  fromEnum (Minute n) = n

instance showMinute :: Show Minute where
  show (Minute m) = "(Minute " <> show m <> ")"

-- | An second component for a time value.
-- |
-- | The constructor is private as values for the type are restricted to the
-- | range 0 to 59, inclusive. The `toEnum` function can be used to safely
-- | acquire an `Second` value from an integer. Correspondingly, a `Second` can
-- | be lowered to a plain integer with the `fromEnum` function.
newtype Second = Second Int

derive newtype instance eqSecond :: Eq Second
derive newtype instance ordSecond :: Ord Second

instance boundedSecond :: Bounded Second where
  bottom = Second 0
  top = Second 59

instance enumSecond :: Enum Second where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumSecond :: BoundedEnum Second where
  cardinality = Cardinality 60
  toEnum n
    | n >= 0 && n <= 59 = Just (Second n)
    | otherwise = Nothing
  fromEnum (Second n) = n

instance showSecond :: Show Second where
  show (Second m) = "(Second " <> show m <> ")"

-- | An millisecond component for a time value.
-- |
-- | The constructor is private as values for the type are restricted to the
-- | range 0 to 999, inclusive. The `toEnum` function can be used to safely
-- | acquire an `Millisecond` value from an integer. Correspondingly, a
-- | `Millisecond` can be lowered to a plain integer with the `fromEnum`
-- | function.
newtype Millisecond = Millisecond Int

derive newtype instance eqMillisecond :: Eq Millisecond
derive newtype instance ordMillisecond :: Ord Millisecond

instance boundedMillisecond :: Bounded Millisecond where
  bottom = Millisecond 0
  top = Millisecond 999

instance enumMillisecond :: Enum Millisecond where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumMillisecond :: BoundedEnum Millisecond where
  cardinality = Cardinality 1000
  toEnum n
    | n >= 0 && n <= 999 = Just (Millisecond n)
    | otherwise = Nothing
  fromEnum (Millisecond n) = n

instance showMillisecond :: Show Millisecond where
  show (Millisecond m) = "(Millisecond " <> show m <> ")"
