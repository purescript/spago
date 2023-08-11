-- | This module has no support of percents and currencies.
-- | Please, note that using simple formatter that tabulates number with
-- | zeros and put commas between thousands should be enough for everything
-- | because one could just compose it with `flip append "%"` or whatever
module Data.Formatter.Number
  ( Formatter(..)
  , printFormatter
  , parseFormatString
  , format
  , unformat
  , formatNumber
  , formatOrShowNumber
  , unformatNumber
  ) where

import Prelude

import Data.Array (many, some)
import Data.Array as Arr
import Data.Either (Either, either)
import Data.Formatter.Internal (foldDigits, repeat)
import Data.Formatter.Parser.Number (parseDigit)
import Data.Formatter.Parser.Utils (runP)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.String.CodeUnits as CU
import Data.Traversable (for)
import Parsing as P
import Parsing.Combinators as PC
import Parsing.String as PS
import Parsing.String.Basic as PSB

newtype Formatter = Formatter
  { comma :: Boolean
  , before :: Int
  , after :: Int
  , abbreviations :: Boolean
  , sign :: Boolean
  }

derive instance genericFormatter :: Generic Formatter _
derive instance newtypeFormatter :: Newtype Formatter _

instance showFormatter :: Show Formatter where
  show = genericShow

derive instance eqFormatter :: Eq Formatter

printFormatter :: Formatter -> String
printFormatter (Formatter f) =
  (if f.sign then "+" else "")
    <> repeat "0" (f.before - one)
    <> (if f.comma then "0,0" else "0")
    <> (if f.after > zero then "." else "")
    <> (repeat "0" f.after)
    <> (if f.abbreviations then "a" else "")

parseFormatString :: String -> Either String Formatter
parseFormatString = runP formatParser

formatParser :: P.Parser String Formatter
formatParser = do
  sign <- PC.optionMaybe $ PC.try $ PS.string "+"
  before <- some $ PS.string "0"
  comma <- PC.optionMaybe $ PC.try $ PS.string ",0"
  dot <- PC.optionMaybe $ PC.try $ PS.string "."
  after <- for dot \_ ->
    PC.try $ many $ PS.string "0"
  abbreviations <- PC.optionMaybe $ PC.try $ PS.string "a"

  pure $ Formatter
    { sign: isJust sign
    , before: Arr.length before
    , comma: isJust comma
    , after: fromMaybe zero $ Arr.length <$> after
    , abbreviations: isJust abbreviations
    }

-- converts a number to a string of the nearest integer _without_ appending ".0" (like `show` for `Number`) or
-- clamping to +/- 2 billion (like when working with `Int`). This is important for performance compared to other
-- means of showing an integer potentially larger than +/- 2 billion.
foreign import showNumberAsInt :: Number -> String

-- | Formats a number according to the format object provided.
-- | Due to the nature of floating point numbers, may yield unpredictable results for extremely
-- | large or extremely small numbers, such as numbers whose absolute values are ≥ 1e21 or ≤ 1e-21,
-- | or when formatting with > 20 digits after the decimal place.
-- | See [purescript-decimals](https://pursuit.purescript.org/packages/purescript-decimals/4.0.0)
-- | for working with arbitrary precision decimals, which supports simple number
-- | formatting for numbers that go beyond the precision available with `Number`.
format :: Formatter -> Number -> String
format (Formatter f) num = do
  let
    absed = Number.abs num
    tens
      | absed > 0.0 = max (Int.floor $ Number.log absed / Number.ln10) 0
      | otherwise = 0

  if f.abbreviations then do
    let
      thousands = tens / 3
      abbr
        | thousands == 0 = ""
        | thousands == 1 = "K"
        | thousands == 2 = "M"
        | thousands == 3 = "G"
        | thousands == 4 = "T"
        | thousands == 5 = "P"
        | thousands == 6 = "E"
        | thousands == 7 = "Z"
        | thousands == 8 = "Y"
        | otherwise = "10e+" <> show (thousands * 3)
      newNum = if thousands < 1 then num else num / Number.pow 1000.0 (Int.toNumber thousands)

    format (Formatter f { abbreviations = false }) newNum <> abbr
  else do
    let
      zeros = f.before - tens - one
      factor = Number.pow 10.0 (Int.toNumber (max 0 f.after))
      rounded = Number.round (absed * factor) / factor
      integer = Number.floor rounded
      leftoverDecimal = rounded - integer
      leftover = Number.round $ leftoverDecimal * factor

      leftoverWithZeros = do
        let
          leftoverString = showNumberAsInt leftover
          leftoverLength = Str.length leftoverString
          zeros' = repeat "0" (f.after - leftoverLength)

        zeros' <> leftoverString

      shownInt =
        if f.comma then
          addCommas [] zero (Arr.reverse (CU.toCharArray (repeat "0" zeros <> showNumberAsInt integer)))
        else
          repeat "0" zeros <> showNumberAsInt integer

      addCommas :: Array Char -> Int -> Array Char -> String
      addCommas acc counter input = case Arr.uncons input of
        Nothing -> CU.fromCharArray acc
        Just { head, tail } | counter < 3 ->
          addCommas (Arr.cons head acc) (counter + one) tail
        _ ->
          addCommas (Arr.cons ',' acc) zero input

      leftovers =
        if f.after < 1 then ""
        else
          "."
            <> (if leftover == 0.0 then repeat "0" f.after else "")
            <> (if leftover > 0.0 then leftoverWithZeros else "")

    (if num < zero then "-" else if num > zero && f.sign then "+" else "")
      <> shownInt
      <> leftovers

unformat :: Formatter -> String -> Either String Number
unformat = runP <<< unformatParser

unformatParser :: Formatter -> P.Parser String Number
unformatParser (Formatter f) = do
  minus <- PC.optionMaybe $ PC.try $ PS.string "-"
  sign <- case minus of
    Nothing | f.sign ->
      (PS.string "+") $> 1.0
    Nothing | otherwise ->
      pure 1.0
    Just _ ->
      pure (-1.0)

  let
    digitsWithCommas :: P.Parser String (Array Int)
    digitsWithCommas =
      if not f.comma then
        some parseDigit <* PS.string "."
      else
        digitsWithCommas' []

    digitsWithCommas' :: Array Int -> P.Parser String (Array Int)
    digitsWithCommas' accum = do
      ds <- some parseDigit

      when (Arr.null accum && Arr.length ds > 3) do
        P.fail "Wrong number of digits between thousand separators"

      when (Arr.length ds /= 3) do
        P.fail "Wrong number of digits between thousand separators"

      sep <- PSB.oneOf [ ',', '.' ]
      case sep of
        '.' -> pure $ accum <> ds
        ',' -> digitsWithCommas' $ accum <> ds
        _ -> P.fail "Incorrect symbol, expected ',' or '.'"

  beforeDigits <- digitsWithCommas
  before <-
    if Arr.length beforeDigits < f.before then
      P.fail "Error: too few digits before dot"
    else
      pure $ Int.toNumber $ foldDigits beforeDigits

  afterDigits <- some parseDigit
  after <-
    if Arr.length afterDigits < f.after then
      P.fail "Error: too few digits after dot"
    else
      pure $ Int.toNumber $ foldDigits afterDigits

  abbr <-
    if f.abbreviations then do
      letter <- PC.optionMaybe $ PC.try $ PSB.oneOf [ 'K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y' ]
      case letter of
        Nothing -> do
          e <- PC.optionMaybe $ PS.string "10e+"
          case e of
            Nothing -> pure 0
            Just _ -> map foldDigits $ many parseDigit
        Just 'K' -> pure 3
        Just 'M' -> pure 6
        Just 'G' -> pure 9
        Just 'T' -> pure 12
        Just 'P' -> pure 15
        Just 'E' -> pure 18
        Just 'Z' -> pure 21
        Just 'Y' -> pure 24
        _ -> pure 0
    else pure 0

  pure $
    Number.pow 10.0 (Int.toNumber abbr)
      * sign
      * (before + after / Number.pow 10.0 (Int.toNumber f.after))

formatNumber :: String -> Number -> Either String String
formatNumber pattern number = parseFormatString pattern <#> flip format number

unformatNumber :: String -> String -> Either String Number
unformatNumber pattern str = parseFormatString pattern >>= flip unformat str

-- Supposed to be used in chaining, because after calling format number there is no
-- good way to extract number back to show.
formatOrShowNumber :: String -> Number -> String
formatOrShowNumber patter number = either (const $ show number) identity $ formatNumber patter number
