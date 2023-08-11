module Data.Formatter.DateTime
  ( Formatter
  , FormatterCommand(..)
  , Meridiem
  , printFormatter
  , printFormatterCommand
  , parseFormatString
  , format
  , formatDateTime
  , unformat
  , unformatDateTime
  , unformatParser
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Apply (applySecond, lift2)
import Control.Lazy as Z
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.State (State, modify_, put, runState)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Date as D
import Data.DateTime as DT
import Data.DateTime.Instant (instant, toDateTime, fromDateTime, unInstant)
import Data.Either (Either(..), either)
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (foldMap, for_)
import Data.Formatter.Internal (foldDigits)
import Data.Formatter.Parser.Number (parseDigit)
import Data.Formatter.Parser.Utils (runP, oneOfAs)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.String.CodeUnits as CU
import Data.Time as T
import Data.Time.Duration as Dur
import Data.Tuple (Tuple(..))
import Parsing as P
import Parsing.Combinators as PC
import Parsing.String as PS
import Parsing.String.Basic as PSB

data FormatterCommand
  = YearFull
  | YearTwoDigits
  | YearAbsolute
  | MonthFull
  | MonthShort
  | MonthTwoDigits
  | DayOfMonthTwoDigits
  | DayOfMonth
  | UnixTimestamp
  | DayOfWeek
  | DayOfWeekName
  | DayOfWeekNameShort
  | Hours24
  | Hours12
  | Meridiem
  | Minutes
  | MinutesTwoDigits
  | Seconds
  | SecondsTwoDigits
  | Milliseconds
  | MillisecondsShort
  | MillisecondsTwoDigits
  | Placeholder String

derive instance eqFormatterCommand :: Eq FormatterCommand
derive instance ordFormatterCommand :: Ord FormatterCommand
derive instance genericFormatter :: Generic FormatterCommand _
instance showFormatter :: Show FormatterCommand where
  show = genericShow

type Formatter = List.List FormatterCommand

printFormatterCommand :: FormatterCommand -> String
printFormatterCommand = case _ of
  YearFull -> "YYYY"
  YearTwoDigits -> "YY"
  YearAbsolute -> "Y"
  MonthFull -> "MMMM"
  MonthShort -> "MMM"
  MonthTwoDigits -> "MM"
  DayOfMonthTwoDigits -> "DD"
  DayOfMonth -> "D"
  UnixTimestamp -> "X"
  DayOfWeek -> "E"
  DayOfWeekName -> "dddd"
  DayOfWeekNameShort -> "ddd"
  Hours24 -> "HH"
  Hours12 -> "hh"
  Meridiem -> "a"
  Minutes -> "m"
  MinutesTwoDigits -> "mm"
  Seconds -> "s"
  SecondsTwoDigits -> "ss"
  MillisecondsShort -> "S"
  MillisecondsTwoDigits -> "SS"
  Milliseconds -> "SSS"
  Placeholder s -> s

printFormatter :: Formatter -> String
printFormatter = foldMap printFormatterCommand

parseFormatString :: String -> Either String Formatter
parseFormatString = runP formatParser

placeholderContent :: P.Parser String String
placeholderContent =
  CU.toCharArray "YMDEHhamsS"
    # PSB.noneOf
    # Array.some
    <#> CU.fromCharArray

formatterCommandParser :: P.Parser String FormatterCommand
formatterCommandParser =
  (PC.try <<< PS.string) `oneOfAs`
    [ Tuple "YYYY" YearFull
    , Tuple "YY" YearTwoDigits
    , Tuple "Y" YearAbsolute
    , Tuple "MMMM" MonthFull
    , Tuple "MMM" MonthShort
    , Tuple "MM" MonthTwoDigits
    , Tuple "DD" DayOfMonthTwoDigits
    , Tuple "D" DayOfMonth
    , Tuple "E" DayOfWeek
    , Tuple "X" UnixTimestamp
    , Tuple "dddd" DayOfWeekName
    , Tuple "ddd" DayOfWeekNameShort
    , Tuple "HH" Hours24
    , Tuple "hh" Hours12
    , Tuple "a" Meridiem
    , Tuple "mm" MinutesTwoDigits
    , Tuple "m" Minutes
    , Tuple "ss" SecondsTwoDigits
    , Tuple "s" Seconds
    , Tuple "SSS" Milliseconds
    , Tuple "SS" MillisecondsTwoDigits
    , Tuple "S" MillisecondsShort
    ] <|> (Placeholder <$> placeholderContent)

formatParser :: P.Parser String Formatter
formatParser = List.some formatterCommandParser

-- | Formatting function that accepts a number that is a year,
-- | and strips away the non-significant digits, leaving only the
-- | ones and tens positions.
formatYearTwoDigits :: Int -> String
formatYearTwoDigits i = case dateLength of
  1 -> "0" <> dateString
  2 -> dateString
  _ -> Str.drop (dateLength - 2) dateString
  where
  dateString = show $ abs i
  dateLength = Str.length $ dateString

fix12 :: Int -> Int
fix12 h = if h == 0 then 12 else h

formatCommand :: DT.DateTime -> FormatterCommand -> String
formatCommand dt@(DT.DateTime d t) = case _ of
  YearFull -> padQuadrupleDigit $ fromEnum $ D.year d
  YearTwoDigits -> formatYearTwoDigits $ fromEnum $ D.year d
  YearAbsolute -> show $ fromEnum $ D.year d
  MonthFull -> show $ D.month d
  MonthShort -> printShortMonth $ D.month d
  MonthTwoDigits -> padSingleDigit $ fromEnum $ D.month d
  DayOfMonthTwoDigits -> padSingleDigit $ fromEnum $ D.day d
  DayOfMonth -> show $ fromEnum $ D.day d
  UnixTimestamp -> show $ Int.floor $ (_ / 1000.0) $ unwrap $ unInstant $ fromDateTime dt
  DayOfWeek -> show $ fromEnum $ D.weekday d
  DayOfWeekName -> show $ D.weekday d
  DayOfWeekNameShort -> Str.take 3 $ show $ D.weekday d
  Hours24 -> padSingleDigit (fromEnum $ T.hour t)
  Hours12 -> padSingleDigit $ fix12 $ (fromEnum $ T.hour t) `mod` 12
  Meridiem -> if (fromEnum $ T.hour t) >= 12 then "PM" else "AM"
  Minutes -> show $ fromEnum $ T.minute t
  MinutesTwoDigits -> padSingleDigit <<< fromEnum $ T.minute t
  Seconds -> show $ fromEnum $ T.second t
  SecondsTwoDigits -> padSingleDigit <<< fromEnum $ T.second t
  Milliseconds -> padDoubleDigit <<< fromEnum $ T.millisecond t
  MillisecondsShort -> show $ (_ / 100) $ fromEnum $ T.millisecond t
  MillisecondsTwoDigits -> padSingleDigit $ (_ / 10) $ fromEnum $ T.millisecond t
  Placeholder s -> s

padSingleDigit :: Int -> String
padSingleDigit i
  | i < 0 = "-" <> padSingleDigit (-i)
  | i < 10 = "0" <> (show i)
  | otherwise = show i

padDoubleDigit :: Int -> String
padDoubleDigit i
  | i < 0 = "-" <> padDoubleDigit (-i)
  | i < 10 = "00" <> (show i)
  | i < 100 = "0" <> (show i)
  | otherwise = show i

padQuadrupleDigit :: Int -> String
padQuadrupleDigit i
  | i < 0 = "-" <> padQuadrupleDigit (-i)
  | i < 10 = "000" <> (show i)
  | i < 100 = "00" <> (show i)
  | i < 1000 = "0" <> (show i)
  | otherwise = show i

format :: Formatter -> DT.DateTime -> String
format f d = foldMap (formatCommand d) f

formatDateTime :: String -> DT.DateTime -> Either String String
formatDateTime pattern datetime =
  parseFormatString pattern <#> (_ `format` datetime)

unformat :: Formatter -> String -> Either String DT.DateTime
unformat = runP <<< unformatParser

data Meridiem = AM | PM

derive instance eqMeridiem :: Eq Meridiem

type UnformatAccum =
  { year :: Maybe Int
  , month :: Maybe Int
  , day :: Maybe Int
  , hour :: Maybe Int
  , minute :: Maybe Int
  , second :: Maybe Int
  , millisecond :: Maybe Int
  , meridiem :: Maybe Meridiem
  }

initialAccum :: UnformatAccum
initialAccum =
  { year: Nothing
  , month: Nothing
  , day: Nothing
  , hour: Nothing
  , minute: Nothing
  , second: Nothing
  , millisecond: Nothing
  , meridiem: Nothing
  }

unformatAccumToDateTime :: UnformatAccum -> Either String DT.DateTime
unformatAccumToDateTime a = applySecond (validAccum a) $
  DT.DateTime
    <$>
      ( D.canonicalDate
          <$> (maybe (Left "Incorrect year") pure $ toEnum $ fromMaybe zero a.year)
          <*> (maybe (Left "Incorrect month") pure $ toEnum $ fromMaybe one a.month)
          <*> (maybe (Left "Incorrect day") pure $ toEnum $ adjustDay a.hour $ fromMaybe one a.day)
      )
    <*>
      ( T.Time
          <$> (maybe (Left "Incorrect hour") pure $ toEnum $ fromMaybe zero $ adjustMeridiem a.meridiem <$> a.hour)
          <*> (maybe (Left "Incorrect minute") pure $ toEnum $ fromMaybe zero a.minute)
          <*> (maybe (Left "Incorrect second") pure $ toEnum $ fromMaybe zero a.second)
          <*> (maybe (Left "Incorrect millisecond") pure $ toEnum $ fromMaybe zero a.millisecond)
      )

validAccum :: UnformatAccum -> Either String Unit
validAccum { hour, minute, second, millisecond } = case hour of
  Just 24 -> for_ [ minute, second, millisecond ] \val ->
    when (fromMaybe 0 val > 0) $ Left "When hour is 24, other time components must be 0"
  _ -> pure unit

adjustDay :: Maybe Int -> Int -> Int
adjustDay (Just 24) n = n + 1
adjustDay _ n = n

adjustMeridiem :: Maybe Meridiem -> Int -> Int
adjustMeridiem (Just AM) 12 = 0
adjustMeridiem (Just PM) 12 = 12
adjustMeridiem (Just PM) n = n + 12
adjustMeridiem (Just AM) n = n
adjustMeridiem Nothing 24 = 0
adjustMeridiem Nothing n = n

exactLength :: forall e. ReaderT { maxLength :: Int, length :: Int | e } (Either String) Unit
exactLength = ask >>= \({ maxLength, length }) -> lift
  if maxLength /= length then
    Left $ "Expected " <> (show maxLength) <> " digits but got " <> (show length)
  else
    Right unit

validateRange :: forall e. Int -> Int -> ReaderT { num :: Int | e } (Either String) Unit
validateRange min max = ask >>= \({ num }) -> lift
  if num < min || num > max then
    Left $ "Number is out of range [ " <> (show min) <> ", " <> (show max) <> " ]"
  else
    Right unit

-- NOTE related discussion: https://github.com/purescript-contrib/purescript-parsing/issues/57
-- | Attempt a computation `n` times, requiring at least one success.
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
takeSome :: forall f a. Alternative f => Z.Lazy (f (List.List a)) => Int -> f a -> f (List.List a)
takeSome 0 _ = pure List.Nil
takeSome n v = List.Cons <$> v <*> Z.defer (\_ -> takeMany (n - 1) v)

-- | Attempt a computation `n` times, returning as many successful results
-- | as possible (possibly zero).
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
takeMany :: forall f a. Alternative f => Z.Lazy (f (List.List a)) => Int -> f a -> f (List.List a)
takeMany 0 _ = pure List.Nil
takeMany n v = takeSome n v <|> pure List.Nil

parseSignedInt
  :: forall m
   . Monad m
  => Int
  -> ReaderT { length :: Int, num :: Int, maxLength :: Int } (Either String) Unit
  -> String
  -> P.ParserT String m Int
parseSignedInt maxLength validators errMsg = do
  isNegative <- isJust <$> PC.optionMaybe (PS.char '-')
  (if isNegative then negate else identity) <$> parseInt maxLength validators errMsg

parseInt
  :: forall m
   . Monad m
  => Int
  -> ReaderT { length :: Int, num :: Int, maxLength :: Int } (Either String) Unit
  -> String
  -> P.ParserT String m Int
parseInt maxLength validators errMsg = do
  ds <- takeSome maxLength parseDigit
  let
    length = List.length ds
    num = foldDigits ds
  case runReaderT validators { length, num, maxLength } of
    Left err -> P.fail $ errMsg <> "(" <> err <> ")"
    Right _ -> pure num

unformatCommandParser :: FormatterCommand -> P.ParserT String (State UnformatAccum) Unit
unformatCommandParser = case _ of
  YearFull -> _ { year = _ } `modifyWithParser`
    (parseSignedInt 4 exactLength "Incorrect full year")
  YearTwoDigits -> _ { year = _ } `modifyWithParser`
    (parseSignedInt 2 exactLength "Incorrect 2-digit year")
  YearAbsolute -> _ { year = _ } `modifyWithParser`
    ( lift2 (*)
        (PC.option 1 $ PC.try $ PS.string "-" <#> (const (-1)))
        (List.some parseDigit <#> foldDigits)
    )
  MonthFull -> _ { month = _ } `modifyWithParser`
    (fromEnum <$> parseMonth)
  MonthShort -> _ { month = _ } `modifyWithParser`
    (fromEnum <$> parseShortMonth)
  MonthTwoDigits -> _ { month = _ } `modifyWithParser`
    (parseInt 2 (validateRange 1 12 <> exactLength) "Incorrect 2-digit month")
  DayOfMonthTwoDigits -> _ { day = _ } `modifyWithParser`
    (parseInt 2 (validateRange 1 31 <> exactLength) "Incorrect day of month")
  DayOfMonth -> _ { day = _ } `modifyWithParser`
    (parseInt 2 (validateRange 1 31) "Incorrect day of month")
  UnixTimestamp -> do
    s <- map foldDigits $ List.some parseDigit
    case map toDateTime $ instant $ Dur.Milliseconds $ 1000.0 * Int.toNumber s of
      Nothing -> P.fail "Incorrect timestamp"
      Just (DT.DateTime d t) -> lift $ put
        { year: Just $ fromEnum $ D.year d
        , month: Just $ fromEnum $ D.month d
        , day: Just $ fromEnum $ D.day d
        , hour: Just $ fromEnum $ T.hour t
        , minute: Just $ fromEnum $ T.minute t
        , second: Just $ fromEnum $ T.second t
        , millisecond: Just $ fromEnum $ T.millisecond t
        , meridiem: Nothing
        }
  -- TODO we would need to use this value if we support date format using week number
  DayOfWeek -> void $ parseInt 1 (validateRange 1 7) "Incorrect day of week"
  DayOfWeekName -> _ { day = _ } `modifyWithParser`
    (fromEnum <$> parseDayOfWeekName)
  DayOfWeekNameShort -> _ { day = _ } `modifyWithParser`
    (fromEnum <$> parseDayOfWeekNameShort)
  Hours24 -> _ { hour = _ } `modifyWithParser`
    (parseInt 2 (validateRange 0 24 <> exactLength) "Incorrect 24 hour")
  Hours12 -> _ { hour = _ } `modifyWithParser`
    (parseInt 2 (validateRange 0 12 <> exactLength) "Incorrect 12 hour")
  Meridiem -> _ { meridiem = _ } `modifyWithParser` parseMeridiem
  MinutesTwoDigits -> _ { minute = _ } `modifyWithParser`
    (parseInt 2 (validateRange 0 59 <> exactLength) "Incorrect 2-digit minute")
  Minutes -> _ { minute = _ } `modifyWithParser`
    (parseInt 2 (validateRange 0 59) "Incorrect minute")
  SecondsTwoDigits -> _ { second = _ } `modifyWithParser`
    (parseInt 2 (validateRange 0 59 <> exactLength) "Incorrect 2-digit second")
  Seconds -> _ { second = _ } `modifyWithParser`
    (parseInt 2 (validateRange 0 59) "Incorrect second")
  Milliseconds -> _ { millisecond = _ } `modifyWithParser`
    (parseInt 3 exactLength "Incorrect millisecond")
  Placeholder s -> void $ PS.string s
  MillisecondsShort -> _ { millisecond = _ } `modifyWithParser`
    (parseInt 1 exactLength "Incorrect 1-digit millisecond" <#> (_ * 100))
  MillisecondsTwoDigits -> _ { millisecond = _ } `modifyWithParser`
    (parseInt 2 exactLength "Incorrect 2-digit millisecond" <#> (_ * 10))
  where
  modifyWithParser :: forall s' s x. (s -> Maybe x -> s) -> P.ParserT s' (State s) x -> P.ParserT s' (State s) Unit
  modifyWithParser f p = do
    v <- p
    lift $ modify_ (flip f (Just v))

unformatParser :: forall m. Monad m => Formatter -> P.ParserT String m DT.DateTime
unformatParser f = do
  acc <- P.mapParserT unState $ foldMap unformatCommandParser f
  either P.fail pure $ unformatAccumToDateTime acc
  where
  unState :: forall x y n. Monad n => State UnformatAccum (Tuple (Either y Unit) x) -> n (Tuple (Either y UnformatAccum) x)
  unState s = case runState s initialAccum of
    Tuple (Tuple e state) res -> pure (Tuple (e $> res) state)

unformatDateTime :: String -> String -> Either String DT.DateTime
unformatDateTime pattern str =
  parseFormatString pattern >>= (_ `unformat` str)

parseMeridiem :: forall m. Monad m => P.ParserT String m Meridiem
parseMeridiem = (PC.try <<< PS.string) `oneOfAs`
  [ Tuple "am" AM
  , Tuple "AM" AM
  , Tuple "pm" PM
  , Tuple "PM" PM
  ]

parseDayOfWeekName :: forall m. Monad m => P.ParserT String m D.Weekday
parseDayOfWeekName = (PC.try <<< PS.string) `oneOfAs`
  [ Tuple "Monday" D.Monday
  , Tuple "Tuesday" D.Tuesday
  , Tuple "Wednesday" D.Wednesday
  , Tuple "Thursday" D.Thursday
  , Tuple "Friday" D.Friday
  , Tuple "Saturday" D.Saturday
  , Tuple "Sunday" D.Sunday
  ]

parseDayOfWeekNameShort :: forall m. Monad m => P.ParserT String m D.Weekday
parseDayOfWeekNameShort = (PC.try <<< PS.string) `oneOfAs`
  [ Tuple "Mon" D.Monday
  , Tuple "Tue" D.Tuesday
  , Tuple "Wed" D.Wednesday
  , Tuple "Thu" D.Thursday
  , Tuple "Fri" D.Friday
  , Tuple "Sat" D.Saturday
  , Tuple "Sun" D.Sunday
  ]

parseMonth :: forall m. Monad m => P.ParserT String m D.Month
parseMonth = (PC.try <<< PS.string) `oneOfAs`
  [ Tuple "January" D.January
  , Tuple "February" D.February
  , Tuple "March" D.March
  , Tuple "April" D.April
  , Tuple "May" D.May
  , Tuple "June" D.June
  , Tuple "July" D.July
  , Tuple "August" D.August
  , Tuple "September" D.September
  , Tuple "October" D.October
  , Tuple "November" D.November
  , Tuple "December" D.December
  ]

parseShortMonth :: forall m. Monad m => P.ParserT String m D.Month
parseShortMonth = (PC.try <<< PS.string) `oneOfAs`
  [ Tuple "Jan" D.January
  , Tuple "Feb" D.February
  , Tuple "Mar" D.March
  , Tuple "Apr" D.April
  , Tuple "May" D.May
  , Tuple "Jun" D.June
  , Tuple "Jul" D.July
  , Tuple "Aug" D.August
  , Tuple "Sep" D.September
  , Tuple "Oct" D.October
  , Tuple "Nov" D.November
  , Tuple "Dec" D.December
  ]

printShortMonth :: D.Month -> String
printShortMonth = case _ of
  D.January -> "Jan"
  D.February -> "Feb"
  D.March -> "Mar"
  D.April -> "Apr"
  D.May -> "May"
  D.June -> "Jun"
  D.July -> "Jul"
  D.August -> "Aug"
  D.September -> "Sep"
  D.October -> "Oct"
  D.November -> "Nov"
  D.December -> "Dec"
