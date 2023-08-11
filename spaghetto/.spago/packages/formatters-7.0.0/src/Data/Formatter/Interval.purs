module Data.Formatter.Interval
  ( unformatRecurringInterval
  , unformatInterval
  , formatRecurringInterval
  , formatInterval
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime as FDT
import Data.Formatter.Parser.Interval (parseRecurringInterval, parseInterval, parseIsoDuration, parseDateTime, extendedDateTimeFormatInUTC)
import Data.Formatter.Parser.Utils (runP)
import Data.Int as Int
import Data.Interval as I
import Data.Interval.Duration.Iso (IsoDuration, unIsoDuration)
import Data.Map (lookup)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))

formatRecurringInterval :: I.RecurringInterval IsoDuration DateTime -> String
formatRecurringInterval (I.RecurringInterval n i) = "R" <> (maybe "" formatInteger n) <> "/" <> (formatInterval i)

formatInterval :: I.Interval IsoDuration DateTime -> String
formatInterval (I.StartEnd x y) = (formatDateTime x) <> "/" <> (formatDateTime y)
formatInterval (I.DurationEnd d x) = (formatIsoDuration d) <> "/" <> (formatDateTime x)
formatInterval (I.StartDuration x d) = (formatDateTime x) <> "/" <> (formatIsoDuration d)
formatInterval (I.DurationOnly d) = (formatIsoDuration d)

formatDateTime :: DateTime -> String
formatDateTime = FDT.format extendedDateTimeFormatInUTC

formatIsoDuration :: IsoDuration -> String
formatIsoDuration = formatDuration <<< unIsoDuration

formatDuration :: I.Duration -> String
formatDuration (I.Duration m) = "P" <> datePart <> timePart
  where
  datePart = componentToString `foldMap` dateComponentsToStr
  timePart = ("T" <> _) `ifmempty` (componentToString `foldMap` timeComponentsToStr)
  ifmempty _ a | a == mempty = mempty
  ifmempty f a = f a
  componentToString (Tuple k s) = maybe "" (formatComponent s) $ lookup k m
  formatComponent designator num = formatNumber num <> designator
  dateComponentsToStr = [ Tuple I.Year "Y", Tuple I.Month "M", Tuple I.Week "W", Tuple I.Day "D" ]
  timeComponentsToStr = [ Tuple I.Hour "H", Tuple I.Minute "M", Tuple I.Second "S" ]

formatInteger :: Int -> String
formatInteger = show

formatNumber :: Number -> String
formatNumber n = if Int.toNumber (Int.floor n) == n then show (Int.floor n) else show n

unformatRecurringInterval :: String -> Either String (I.RecurringInterval IsoDuration DateTime)
unformatRecurringInterval = runP $ parseRecurringInterval parseIsoDuration parseDateTime

unformatInterval :: String -> Either String (I.Interval IsoDuration DateTime)
unformatInterval = runP $ parseInterval parseIsoDuration parseDateTime
