module Data.String.NonEmpty.CodeUnits
  ( fromCharArray
  , fromNonEmptyCharArray
  , singleton
  , cons
  , snoc
  , fromFoldable1
  , toCharArray
  , toNonEmptyCharArray
  , charAt
  , toChar
  , indexOf
  , indexOf'
  , lastIndexOf
  , lastIndexOf'
  , uncons
  , length
  , take
  , takeRight
  , takeWhile
  , drop
  , dropRight
  , dropWhile
  , countPrefix
  , splitAt
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromJust)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Foldable as F1
import Data.String.CodeUnits as CU
import Data.String.NonEmpty.Internal (NonEmptyString(..), fromString)
import Data.String.Pattern (Pattern)
import Data.String.Unsafe as U
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- For internal use only. Do not export.
toNonEmptyString :: String -> NonEmptyString
toNonEmptyString = NonEmptyString

-- For internal use only. Do not export.
fromNonEmptyString :: NonEmptyString -> String
fromNonEmptyString (NonEmptyString s) = s

-- For internal use only. Do not export.
liftS :: forall r. (String -> r) -> NonEmptyString -> r
liftS f (NonEmptyString s) = f s

-- | Creates a `NonEmptyString` from a character array `String`, returning
-- | `Nothing` if the input is empty.
-- |
-- | ```purescript
-- | fromCharArray [] = Nothing
-- | fromCharArray ['a', 'b', 'c'] = Just (NonEmptyString "abc")
-- | ```
fromCharArray :: Array Char -> Maybe NonEmptyString
fromCharArray = case _ of
  [] -> Nothing
  cs -> Just (toNonEmptyString (CU.fromCharArray cs))

fromNonEmptyCharArray :: NonEmptyArray Char -> NonEmptyString
fromNonEmptyCharArray = unsafePartial fromJust <<< fromCharArray <<< NEA.toArray

-- | Creates a `NonEmptyString` from a character.
singleton :: Char -> NonEmptyString
singleton = toNonEmptyString <<< CU.singleton

-- | Creates a `NonEmptyString` from a string by prepending a character.
-- |
-- | ```purescript
-- | cons 'a' "bc" = NonEmptyString "abc"
-- | cons 'a' "" = NonEmptyString "a"
-- | ```
cons :: Char -> String -> NonEmptyString
cons c s = toNonEmptyString (CU.singleton c <> s)

-- | Creates a `NonEmptyString` from a string by appending a character.
-- |
-- | ```purescript
-- | snoc 'c' "ab" = NonEmptyString "abc"
-- | snoc 'a' "" = NonEmptyString "a"
-- | ```
snoc :: Char -> String -> NonEmptyString
snoc c s = toNonEmptyString (s <> CU.singleton c)

-- | Creates a `NonEmptyString` from a `Foldable1` container carrying
-- | characters.
fromFoldable1 :: forall f. Foldable1 f => f Char -> NonEmptyString
fromFoldable1 = F1.fold1 <<< coe
  where
    coe ∷ f Char -> f NonEmptyString
    coe = unsafeCoerce

-- | Converts the `NonEmptyString` into an array of characters.
-- |
-- | ```purescript
-- | toCharArray (NonEmptyString "Hello☺\n") == ['H','e','l','l','o','☺','\n']
-- | ```
toCharArray :: NonEmptyString -> Array Char
toCharArray = CU.toCharArray <<< fromNonEmptyString

-- | Converts the `NonEmptyString` into a non-empty array of characters.
toNonEmptyCharArray :: NonEmptyString -> NonEmptyArray Char
toNonEmptyCharArray = unsafePartial fromJust <<< NEA.fromArray <<< toCharArray

-- | Returns the character at the given index, if the index is within bounds.
-- |
-- | ```purescript
-- | charAt 2 (NonEmptyString "Hello") == Just 'l'
-- | charAt 10 (NonEmptyString "Hello") == Nothing
-- | ```
charAt :: Int -> NonEmptyString -> Maybe Char
charAt = liftS <<< CU.charAt

-- | Converts the `NonEmptyString` to a character, if the length of the string
-- | is exactly `1`.
-- |
-- | ```purescript
-- | toChar "H" == Just 'H'
-- | toChar "Hi" == Nothing
-- | ```
toChar :: NonEmptyString -> Maybe Char
toChar = CU.toChar <<< fromNonEmptyString

-- | Returns the index of the first occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | indexOf (Pattern "c") (NonEmptyString "abcdc") == Just 2
-- | indexOf (Pattern "c") (NonEmptyString "aaa") == Nothing
-- | ```
indexOf :: Pattern -> NonEmptyString -> Maybe Int
indexOf = liftS <<< CU.indexOf

-- | Returns the index of the first occurrence of the pattern in the
-- | given string, starting at the specified index. Returns `Nothing` if there is
-- | no match.
-- |
-- | ```purescript
-- | indexOf' (Pattern "a") 2 (NonEmptyString "ababa") == Just 2
-- | indexOf' (Pattern "a") 3 (NonEmptyString "ababa") == Just 4
-- | ```
indexOf' :: Pattern -> Int -> NonEmptyString -> Maybe Int
indexOf' pat = liftS <<< CU.indexOf' pat

-- | Returns the index of the last occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | lastIndexOf (Pattern "c") (NonEmptyString "abcdc") == Just 4
-- | lastIndexOf (Pattern "c") (NonEmptyString "aaa") == Nothing
-- | ```
lastIndexOf :: Pattern -> NonEmptyString -> Maybe Int
lastIndexOf = liftS <<< CU.lastIndexOf

-- | Returns the index of the last occurrence of the pattern in the
-- | given string, starting at the specified index and searching
-- | backwards towards the beginning of the string.
-- |
-- | Starting at a negative index is equivalent to starting at 0 and
-- | starting at an index greater than the string length is equivalent
-- | to searching in the whole string.
-- |
-- | Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | lastIndexOf' (Pattern "a") (-1) (NonEmptyString "ababa") == Just 0
-- | lastIndexOf' (Pattern "a") 1 (NonEmptyString "ababa") == Just 0
-- | lastIndexOf' (Pattern "a") 3 (NonEmptyString "ababa") == Just 2
-- | lastIndexOf' (Pattern "a") 4 (NonEmptyString "ababa") == Just 4
-- | lastIndexOf' (Pattern "a") 5 (NonEmptyString "ababa") == Just 4
-- | ```
lastIndexOf' :: Pattern -> Int -> NonEmptyString -> Maybe Int
lastIndexOf' pat = liftS <<< CU.lastIndexOf' pat

-- | Returns the first character and the rest of the string.
-- |
-- | ```purescript
-- | uncons "a" == { head: 'a', tail: Nothing }
-- | uncons "Hello World" == { head: 'H', tail: Just (NonEmptyString "ello World") }
-- | ```
uncons :: NonEmptyString -> { head :: Char, tail :: Maybe NonEmptyString }
uncons nes =
  let
    s = fromNonEmptyString nes
  in
    { head: U.charAt 0 s
    , tail: fromString (CU.drop 1 s)
    }

-- | Returns the number of characters the string is composed of.
-- |
-- | ```purescript
-- | length (NonEmptyString "Hello World") == 11
-- | ```
length :: NonEmptyString -> Int
length = CU.length <<< fromNonEmptyString

-- | Returns the first `n` characters of the string. Returns `Nothing` if `n` is
-- | less than 1.
-- |
-- | ```purescript
-- | take 5 (NonEmptyString "Hello World") == Just (NonEmptyString "Hello")
-- | take 0 (NonEmptyString "Hello World") == Nothing
-- | ```
take :: Int -> NonEmptyString -> Maybe NonEmptyString
take i nes =
  let
    s = fromNonEmptyString nes
  in
    if i < 1
      then Nothing
      else Just (toNonEmptyString (CU.take i s))

-- | Returns the last `n` characters of the string. Returns `Nothing` if `n` is
-- | less than 1.
-- |
-- | ```purescript
-- | take 5 (NonEmptyString "Hello World") == Just (NonEmptyString "World")
-- | take 0 (NonEmptyString "Hello World") == Nothing
-- | ```
takeRight :: Int -> NonEmptyString -> Maybe NonEmptyString
takeRight i nes =
  let
    s = fromNonEmptyString nes
  in
    if i < 1
      then Nothing
      else Just (toNonEmptyString (CU.takeRight i s))

-- | Returns the longest prefix of characters that satisfy the predicate.
-- | `Nothing` is returned if there is no matching prefix.
-- |
-- | ```purescript
-- | takeWhile (_ /= ':') (NonEmptyString "http://purescript.org") == Just (NonEmptyString "http")
-- | takeWhile (_ == 'a') (NonEmptyString "xyz") == Nothing
-- | ```
takeWhile :: (Char -> Boolean) -> NonEmptyString -> Maybe NonEmptyString
takeWhile f = fromString <<< liftS (CU.takeWhile f)

-- | Returns the string without the first `n` characters. Returns `Nothing` if
-- | more characters are dropped than the string is long.
-- |
-- | ```purescript
-- | drop 6 (NonEmptyString "Hello World") == Just (NonEmptyString "World")
-- | drop 20 (NonEmptyString "Hello World") == Nothing
-- | ```
drop :: Int -> NonEmptyString -> Maybe NonEmptyString
drop i nes =
  let
    s = fromNonEmptyString nes
  in
    if i >= CU.length s
      then Nothing
      else Just (toNonEmptyString (CU.drop i s))

-- | Returns the string without the last `n` characters. Returns `Nothing` if
-- | more characters are dropped than the string is long.
-- |
-- | ```purescript
-- | dropRight 6 (NonEmptyString "Hello World") == Just (NonEmptyString "Hello")
-- | dropRight 20 (NonEmptyString "Hello World") == Nothing
-- | ```
dropRight :: Int -> NonEmptyString -> Maybe NonEmptyString
dropRight i nes =
  let
    s = fromNonEmptyString nes
  in
    if i >= CU.length s
      then Nothing
      else Just (toNonEmptyString (CU.dropRight i s))

-- | Returns the suffix remaining after `takeWhile`.
-- |
-- | ```purescript
-- | dropWhile (_ /= '.') (NonEmptyString "Test.purs") == Just (NonEmptyString ".purs")
-- | ```
dropWhile :: (Char -> Boolean) -> NonEmptyString -> Maybe NonEmptyString
dropWhile f = fromString <<< liftS (CU.dropWhile f)

-- | Returns the number of contiguous characters at the beginning of the string
-- | for which the predicate holds.
-- |
-- | ```purescript
-- | countPrefix (_ /= 'o') (NonEmptyString "Hello World") == 4
-- | ```
countPrefix :: (Char -> Boolean) -> NonEmptyString -> Int
countPrefix = liftS <<< CU.countPrefix

-- | Returns the substrings of a split at the given index, if the index is
-- | within bounds.
-- |
-- | ```purescript
-- | splitAt 2 (NonEmptyString "Hello World") == Just { before: Just (NonEmptyString "He"), after: Just (NonEmptyString "llo World") }
-- | splitAt 10 (NonEmptyString "Hi") == Nothing
-- | ```
splitAt
  :: Int
  -> NonEmptyString
  -> { before :: Maybe NonEmptyString, after :: Maybe NonEmptyString }
splitAt i nes =
  case CU.splitAt i (fromNonEmptyString nes) of
    { before, after } -> { before: fromString before, after: fromString after }
