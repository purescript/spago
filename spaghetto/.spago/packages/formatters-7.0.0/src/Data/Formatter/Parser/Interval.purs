module Data.Formatter.Parser.Interval
  ( parseRecurringInterval
  , parseInterval
  , parseIsoDuration
  , parseDateTime
  , extendedDateTimeFormatInUTC
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, fold, foldMap, intercalate)
import Data.Formatter.DateTime (unformatParser, Formatter, parseFormatString)
import Data.Formatter.Parser.Number (parseNumber, parseMaybeInteger)
import Data.Interval as I
import Data.Interval.Duration.Iso (IsoDuration, mkIsoDuration, prettyError)
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafeCrashWith)
import Parsing as P
import Parsing.Combinators as PC
import Parsing.String as PS

parseRecurringInterval :: forall a b. P.Parser String a -> P.Parser String b -> P.Parser String (I.RecurringInterval a b)
parseRecurringInterval duration date =
  I.RecurringInterval <$> (PS.string "R" *> parseMaybeInteger) <*> (PS.string "/" *> parseInterval duration date)

parseInterval :: forall a b. P.Parser String a -> P.Parser String b -> P.Parser String (I.Interval a b)
parseInterval duration date = [ startEnd, durationEnd, startDuration, durationOnly ] <#> PC.try # PC.choice
  where
  startEnd = I.StartEnd <$> date <* PS.string "/" <*> date
  durationEnd = I.DurationEnd <$> duration <* PS.string "/" <*> date
  startDuration = I.StartDuration <$> date <* PS.string "/" <*> duration
  durationOnly = I.DurationOnly <$> duration

parseIsoDuration :: P.Parser String IsoDuration
parseIsoDuration = do
  dur <- parseDuration
  case mkIsoDuration dur of
    Left errs -> do
      let errorStr = intercalate ", " (prettyError <$> errs)
      P.fail $ "Extracted Duration is not valid ISO duration (" <> errorStr <> ")"
    Right a -> pure a

parseDuration :: P.Parser String I.Duration
parseDuration = PS.string "P" *> (weekDuration <|> fullDuration)
  where
  weekDuration = mkComponentsParser [ Tuple I.week "W" ]
  fullDuration = (append <$> durationDatePart <*> durationTimePart) `failIfEmpty` "Must contain valid duration components"
  durationDatePart = PC.option mempty $ PC.try $ mkComponentsParser [ Tuple I.year "Y", Tuple I.month "M", Tuple I.day "D" ]
  durationTimePart = PC.option mempty $ (PC.try $ PS.string "T") *> (mkComponentsParser [ Tuple I.hour "H", Tuple I.minute "M", Tuple I.second "S" ])

failIfEmpty :: forall a. Monoid a => Eq a => P.Parser String a -> String -> P.Parser String a
failIfEmpty p str = p >>= \x -> if x == mempty then P.fail str else pure x

mkComponentsParser :: Array (Tuple (Number -> I.Duration) String) -> P.Parser String I.Duration
mkComponentsParser arr = p `failIfEmpty` ("None of valid duration components (" <> (show $ snd <$> arr) <> ") were present")
  where
  p = arr <#> applyDurations # sequence <#> foldFoldableMaybe

  applyDurations :: Tuple (Number -> I.Duration) String -> P.Parser String (Maybe I.Duration)
  applyDurations (Tuple f c) = PC.optionMaybe $ PC.try (f <$> component c)

  foldFoldableMaybe :: forall f a. Foldable f => Monoid a => f (Maybe a) -> a
  foldFoldableMaybe = foldMap fold

  component :: String -> P.Parser String Number
  component designator = parseNumber <* PS.string designator

-- parser for DateTime in UTC time zone using "extended format"
parseDateTime :: forall m. Monad m => P.ParserT String m DateTime
parseDateTime = unformatParser extendedDateTimeFormatInUTC

extendedDateTimeFormatInUTC :: Formatter
extendedDateTimeFormatInUTC = parseFormatString "YYYY-MM-DDTHH:mm:ssZ"
  # either unsafeCrashWith identity -- the format must be valid ISO date format
