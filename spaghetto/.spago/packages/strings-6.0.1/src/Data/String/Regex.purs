-- | Wraps Javascript's `RegExp` object that enables matching strings with
-- | patterns defined by regular expressions.
-- | For details of the underlying implementation, see [RegExp Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp).
module Data.String.Regex
  ( Regex(..)
  , regex
  , source
  , flags
  , renderFlags
  , parseFlags
  , test
  , match
  , replace
  , replace'
  , search
  , split
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex.Flags (RegexFlags(..), RegexFlagsRec)

-- | Wraps Javascript `RegExp` objects.
foreign import data Regex :: Type

foreign import showRegexImpl :: Regex -> String

instance showRegex :: Show Regex where
  show = showRegexImpl

foreign import regexImpl
  :: (String -> Either String Regex)
  -> (Regex -> Either String Regex)
  -> String
  -> String
  -> Either String Regex

-- | Constructs a `Regex` from a pattern string and flags. Fails with
-- | `Left error` if the pattern contains a syntax error.
regex :: String -> RegexFlags -> Either String Regex
regex s f = regexImpl Left Right s $ renderFlags f

-- | Returns the pattern string used to construct the given `Regex`.
foreign import source :: Regex -> String

-- | Returns the `RegexFlags` used to construct the given `Regex`.
flags :: Regex -> RegexFlags
flags = RegexFlags <<< flagsImpl

-- | Returns the `RegexFlags` inner record used to construct the given `Regex`.
foreign import flagsImpl :: Regex -> RegexFlagsRec

-- | Returns the string representation of the given `RegexFlags`.
renderFlags :: RegexFlags -> String
renderFlags (RegexFlags f) =
  (if f.global then "g" else "") <>
  (if f.ignoreCase then "i" else "") <>
  (if f.multiline then "m" else "") <>
  (if f.dotAll then "s" else "") <>
  (if f.sticky then "y" else "") <>
  (if f.unicode then "u" else "")

-- | Parses the string representation of `RegexFlags`.
parseFlags :: String -> RegexFlags
parseFlags s = RegexFlags
  { global: contains (Pattern "g") s
  , ignoreCase: contains (Pattern "i") s
  , multiline: contains (Pattern "m") s
  , dotAll: contains (Pattern "s") s
  , sticky: contains (Pattern "y") s
  , unicode: contains (Pattern "u") s
  }

-- | Returns `true` if the `Regex` matches the string. In contrast to
-- | `RegExp.prototype.test()` in JavaScript, `test` does not affect
-- | the `lastIndex` property of the Regex.
foreign import test :: Regex -> String -> Boolean

foreign import _match
  :: (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> Regex
  -> String
  -> Maybe (NonEmptyArray (Maybe String))

-- | Matches the string against the `Regex` and returns an array of matches
-- | if there were any. Each match has type `Maybe String`, where `Nothing`
-- | represents an unmatched optional capturing group.
-- | See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match).
match :: Regex -> String -> Maybe (NonEmptyArray (Maybe String))
match = _match Just Nothing

-- | Replaces occurrences of the `Regex` with the first string. The replacement
-- | string can include special replacement patterns escaped with `"$"`.
-- | See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace).
foreign import replace :: Regex -> String -> String -> String

foreign import _replaceBy
  :: (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> Regex
  -> (String -> Array (Maybe String) -> String)
  -> String
  -> String

-- | Transforms occurrences of the `Regex` using a function of the matched
-- | substring and a list of captured substrings of type `Maybe String`,
-- | where `Nothing` represents an unmatched optional capturing group.
-- | See the [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter).
replace' :: Regex -> (String -> Array (Maybe String) -> String) -> String -> String
replace' = _replaceBy Just Nothing

foreign import _search
  :: (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> Regex
  -> String
  -> Maybe Int

-- | Returns `Just` the index of the first match of the `Regex` in the string,
-- | or `Nothing` if there is no match.
search :: Regex -> String -> Maybe Int
search = _search Just Nothing

-- | Split the string into an array of substrings along occurrences of the `Regex`.
foreign import split :: Regex -> String -> Array String
