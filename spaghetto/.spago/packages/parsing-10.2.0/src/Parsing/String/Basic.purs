-- | Basic `String` parsers derived from primitive `String` parsers.
-- |
-- | #### unicode dependency
-- |
-- | Some of the parsers in this module depend on the
-- | [__unicode__](https://pursuit.purescript.org/packages/purescript-unicode)
-- | package.
-- | The __unicode__ package is large; about half a megabyte unminified.
-- | If code which depends on __parsing__ is “tree-shaken”
-- | “dead-code-eliminated,” then
-- | all of the __unicode__ package will be eliminated.
-- |
-- | The __unicode__-dependent parsers in this module will call functions
-- | which use large lookup tables from the __unicode__ package.
-- | Using any of these __unicode__-dependent parsers
-- | may result in a minified, dead-code-eliminated bundle size increase
-- | of over 100 kilobytes.
module Parsing.String.Basic
  ( digit
  , hexDigit
  , octDigit
  , letter
  , space
  , lower
  , upper
  , alphaNum
  , intDecimal
  , number
  , takeWhile
  , takeWhile1
  , whiteSpace
  , skipSpaces
  , oneOf
  , oneOfCodePoints
  , noneOf
  , noneOfCodePoints
  ) where

import Prelude

import Data.Array (elem, notElem)
import Data.CodePoint.Unicode (isAlpha, isAlphaNum, isDecDigit, isHexDigit, isLower, isOctDigit, isSpace, isUpper)
import Data.Either (Either(..), either)
import Data.Int as Data.Int
import Data.Maybe (Maybe(..))
import Data.Number (infinity, nan)
import Data.Number as Data.Number
import Data.String (CodePoint, singleton)
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits as SCU
import Parsing (ParserT, fail)
import Parsing.Combinators (choice, tryRethrow, (<?>), (<|>), (<~?>))
import Parsing.String (consumeWith, regex, satisfy, satisfyCodePoint, string)
import Partial.Unsafe (unsafeCrashWith)

-- | Parse a digit.  Matches any char that satisfies `Data.CodePoint.Unicode.isDecDigit`.
digit :: forall m. ParserT String m Char
digit = satisfyCP isDecDigit <?> "digit"

-- | Parse a hex digit.  Matches any char that satisfies `Data.CodePoint.Unicode.isHexDigit`.
hexDigit :: forall m. ParserT String m Char
hexDigit = satisfyCP isHexDigit <?> "hex digit"

-- | Parse an octal digit.  Matches any char that satisfies `Data.CodePoint.Unicode.isOctDigit`.
octDigit :: forall m. ParserT String m Char
octDigit = satisfyCP isOctDigit <?> "oct digit"

-- | Parse a lowercase letter.  Matches any char that satisfies `Data.CodePoint.Unicode.isLower`.
lower :: forall m. ParserT String m Char
lower = satisfyCP isLower <?> "lowercase letter"

-- | Parse an uppercase letter.  Matches any char that satisfies `Data.CodePoint.Unicode.isUpper`.
upper :: forall m. ParserT String m Char
upper = satisfyCP isUpper <?> "uppercase letter"

-- | Parse a space character.  Matches any char that satisfies `Data.CodePoint.Unicode.isSpace`.
space :: forall m. ParserT String m Char
space = satisfyCP isSpace <?> "space"

-- | Parse an alphabetical character.  Matches any char that satisfies `Data.CodePoint.Unicode.isAlpha`.
letter :: forall m. ParserT String m Char
letter = satisfyCP isAlpha <?> "letter"

-- | Parse an alphabetical or numerical character.
-- | Matches any char that satisfies `Data.CodePoint.Unicode.isAlphaNum`.
alphaNum :: forall m. ParserT String m Char
alphaNum = satisfyCP isAlphaNum <?> "letter or digit"

-- | Parser based on the __Data.Number.fromString__ function.
-- |
-- | This should be the inverse of `show :: Number -> String`.
-- |
-- | Examples of strings which can be parsed by this parser:
-- | * `"3"`
-- | * `"3.0"`
-- | * `".3"`
-- | * `"-0.3"`
-- | * `"+0.3"`
-- | * `"-3e-1"`
-- | * `"-3.0E-1.0"`
-- | * `"NaN"`
-- | * `"-Infinity"`
number :: forall m. ParserT String m Number
number =
  choice
    [ string "Infinity" *> pure infinity
    , string "+Infinity" *> pure infinity
    , string "-Infinity" *> pure (negate infinity)
    , string "NaN" *> pure nan
    , tryRethrow $ do
        -- This primitiv-ish parser should always backtrack on fail.
        -- Currently regex allows some illegal inputs, like "."
        -- The important thing is that the regex will find the correct
        -- boundary of a candidate string to pass to fromString.
        section <- numberRegex
        -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseFloat
        case Data.Number.fromString section of
          Nothing -> fail "Expected Number"
          Just x -> pure x
    ] <|> fail "Expected Number"

-- Non-exported regex is compiled at startup time.
numberRegex :: forall m. ParserT String m String
numberRegex = either unsafeCrashWith identity $ regex pattern mempty
  where
  pattern = "[+-]?[0-9]*(\\.[0-9]*)?([eE][+-]?[0-9]*(\\.[0-9]*)?)?"

-- | Parser based on the __Data.Int.fromString__ function.
-- |
-- | This should be the inverse of `show :: Int -> String`.
-- |
-- | Examples of strings which can be parsed by this parser:
-- | * `"3"`
-- | * `"-3"`
-- | * `"+300"`
intDecimal :: forall m. ParserT String m Int
intDecimal = tryRethrow do
  section <- intDecimalRegex <|> fail "Expected Int"
  case Data.Int.fromString section of
    Nothing -> fail "Expected Int"
    Just x -> pure x

-- Non-exported regex is compiled at startup time.
intDecimalRegex :: forall m. ParserT String m String
intDecimalRegex = either unsafeCrashWith identity $ regex pattern mempty
  where
  pattern = "[+-]?[0-9]+"

-- | Helper function
satisfyCP :: forall m. (CodePoint -> Boolean) -> ParserT String m Char
satisfyCP p = satisfy (p <<< codePointFromChar)

-- | Match zero or more whitespace characters satisfying
-- | `Data.CodePoint.Unicode.isSpace`.
-- |
-- | Always succeeds. Will consume only when matched whitespace string
-- | is non-empty.
whiteSpace :: forall m. ParserT String m String
whiteSpace = takeWhile isSpace

-- | Skip whitespace characters satisfying `Data.CodePoint.Unicode.isSpace`
-- | and throw them away.
-- |
-- | Always succeeds. Will only consume when some characters are skipped.
skipSpaces :: forall m. ParserT String m Unit
skipSpaces = void whiteSpace

-- | Match one of the BMP `Char`s in the array.
oneOf :: forall m. Array Char -> ParserT String m Char
oneOf ss = satisfy (flip elem ss) <~?> \_ -> "one of " <> show ss

-- | Match any BMP `Char` not in the array.
noneOf :: forall m. Array Char -> ParserT String m Char
noneOf ss = satisfy (flip notElem ss) <~?> \_ -> "none of " <> show ss

-- | Match one of the Unicode characters in the array.
oneOfCodePoints :: forall m. Array CodePoint -> ParserT String m CodePoint
oneOfCodePoints ss = satisfyCodePoint (flip elem ss) <~?> \_ -> "one of " <> show (singleton <$> ss)

-- | Match any Unicode character not in the array.
noneOfCodePoints :: forall m. Array CodePoint -> ParserT String m CodePoint
noneOfCodePoints ss = satisfyCodePoint (flip notElem ss) <~?> \_ -> "none of " <> show (singleton <$> ss)

-- | Take the longest `String` for which the characters satisfy the
-- | predicate.
-- |
-- | See [__`Data.CodePoint.Unicode`__](https://pursuit.purescript.org/packages/purescript-unicode/docs/Data.CodePoint.Unicode)
-- | for useful predicates.
-- |
-- | Example:
-- |
-- | ```
-- | runParser "Tackling the Awkward Squad" do
-- |   takeWhile Data.CodePoint.Unicode.isLetter
-- | ```
-- | ---
-- | ```
-- | Right "Tackling"
-- | ```
-- |
-- | You should prefer `takeWhile isLetter` to
-- | `fromCharArray <$> Data.Array.many letter`.
takeWhile :: forall m. (CodePoint -> Boolean) -> ParserT String m String
takeWhile predicate =
  consumeWith \s ->
    let
      value = String.takeWhile predicate s
    in
      Right
        { consumed: value
        , remainder: SCU.drop (SCU.length value) s
        , value
        }

-- | Take the longest `String` for which the characters satisfy the
-- | predicate. Require at least 1 character. You should supply an
-- | expectation description for the error
-- | message for when the predicate fails on the first character.
-- |
-- | See [__`Data.CodePoint.Unicode`__](https://pursuit.purescript.org/packages/purescript-unicode/docs/Data.CodePoint.Unicode)
-- | for useful predicates.
-- |
-- | Example:
-- |
-- | ```
-- | runParser "Tackling the Awkward Squad" do
-- |   takeWhile1 Data.CodePoint.Unicode.isLetter <?> "a letter"
-- | ```
-- | ---
-- | ```
-- | Right "Tackling"
-- | ```
takeWhile1 :: forall m. (CodePoint -> Boolean) -> ParserT String m String
takeWhile1 predicate =
  consumeWith \s ->
    let
      value = String.takeWhile predicate s
      len = SCU.length value
    in
      if len > 0 then Right
        { consumed: value
        , remainder: SCU.drop (SCU.length value) s
        , value
        }
      else Left "Expected character satisfying predicate"
