-- | These functions allow PureScript strings to be treated as if they were
-- | sequences of Unicode code points instead of their true underlying
-- | implementation (sequences of UTF-16 code units). For nearly all uses of
-- | strings, these functions should be preferred over the ones in
-- | `Data.String.CodeUnits`.
module Data.String.CodePoints
  ( module Exports
  , CodePoint
  , codePointFromChar
  , singleton
  , fromCodePointArray
  , toCodePointArray
  , codePointAt
  , uncons
  , length
  , countPrefix
  , indexOf
  , indexOf'
  , lastIndexOf
  , lastIndexOf'
  , take
  -- , takeRight
  , takeWhile
  , drop
  -- , dropRight
  , dropWhile
  -- , slice
  , splitAt
  ) where

import Prelude

import Data.Array as Array
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc, fromEnum, toEnum, toEnumWithDefaults)
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (contains, stripPrefix, stripSuffix) as Exports
import Data.String.CodeUnits as CU
import Data.String.Common (toUpper)
import Data.String.Pattern (Pattern)
import Data.String.Unsafe as Unsafe
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)

-- | CodePoint is an `Int` bounded between `0` and `0x10FFFF`, corresponding to
-- | Unicode code points.
newtype CodePoint = CodePoint Int

derive instance eqCodePoint :: Eq CodePoint
derive instance ordCodePoint :: Ord CodePoint

instance showCodePoint :: Show CodePoint where
  show (CodePoint i) = "(CodePoint 0x" <> toUpper (toStringAs hexadecimal i) <> ")"

instance boundedCodePoint :: Bounded CodePoint where
  bottom = CodePoint 0
  top = CodePoint 0x10FFFF

instance enumCodePoint :: Enum CodePoint where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance boundedEnumCodePoint :: BoundedEnum CodePoint where
  cardinality = Cardinality (0x10FFFF + 1)
  fromEnum (CodePoint n) = n
  toEnum n
    | n >= 0 && n <= 0x10FFFF = Just (CodePoint n)
    | otherwise = Nothing

-- | Creates a `CodePoint` from a given `Char`.
-- |
-- | ```purescript
-- | >>> codePointFromChar 'B'
-- | CodePoint 0x42 -- represents 'B'
-- | ```
-- |
codePointFromChar :: Char -> CodePoint
codePointFromChar = fromEnum >>> CodePoint

-- | Creates a string containing just the given code point. Operates in
-- | constant space and time.
-- |
-- | ```purescript
-- | >>> map singleton (toEnum 0x1D400)
-- | Just "𝐀"
-- | ```
-- |
singleton :: CodePoint -> String
singleton = _singleton singletonFallback

foreign import _singleton
  :: (CodePoint -> String)
  -> CodePoint
  -> String

singletonFallback :: CodePoint -> String
singletonFallback (CodePoint cp) | cp <= 0xFFFF = fromCharCode cp
singletonFallback (CodePoint cp) =
  let lead = ((cp - 0x10000) / 0x400) + 0xD800 in
  let trail = (cp - 0x10000) `mod` 0x400 + 0xDC00 in
  fromCharCode lead <> fromCharCode trail

-- | Creates a string from an array of code points. Operates in space and time
-- | linear to the length of the array.
-- |
-- | ```purescript
-- | >>> codePointArray = toCodePointArray "c 𝐀"
-- | >>> codePointArray
-- | [CodePoint 0x63, CodePoint 0x20, CodePoint 0x1D400]
-- | >>> fromCodePointArray codePointArray
-- | "c 𝐀"
-- | ```
-- |
fromCodePointArray :: Array CodePoint -> String
fromCodePointArray = _fromCodePointArray singletonFallback

foreign import _fromCodePointArray
  :: (CodePoint -> String)
  -> Array CodePoint
  -> String

-- | Creates an array of code points from a string. Operates in space and time
-- | linear to the length of the string.
-- |
-- | ```purescript
-- | >>> codePointArray = toCodePointArray "b 𝐀𝐀"
-- | >>> codePointArray
-- | [CodePoint 0x62, CodePoint 0x20, CodePoint 0x1D400, CodePoint 0x1D400]
-- | >>> map singleton codePointArray
-- | ["b", " ", "𝐀", "𝐀"]
-- | ```
-- |
toCodePointArray :: String -> Array CodePoint
toCodePointArray = _toCodePointArray toCodePointArrayFallback unsafeCodePointAt0

foreign import _toCodePointArray
  :: (String -> Array CodePoint)
  -> (String -> CodePoint)
  -> String
  -> Array CodePoint

toCodePointArrayFallback :: String -> Array CodePoint
toCodePointArrayFallback s = unfoldr unconsButWithTuple s

unconsButWithTuple :: String -> Maybe (Tuple CodePoint String)
unconsButWithTuple s = (\{ head, tail } -> Tuple head tail) <$> uncons s

-- | Returns the first code point of the string after dropping the given number
-- | of code points from the beginning, if there is such a code point. Operates
-- | in constant space and in time linear to the given index.
-- |
-- | ```purescript
-- | >>> codePointAt 1 "𝐀𝐀𝐀𝐀"
-- | Just (CodePoint 0x1D400) -- represents "𝐀"
-- | -- compare to Data.String:
-- | >>> charAt 1 "𝐀𝐀𝐀𝐀"
-- | Just '�'
-- | ```
-- |
codePointAt :: Int -> String -> Maybe CodePoint
codePointAt n _ | n < 0 = Nothing
codePointAt 0 "" = Nothing
codePointAt 0 s = Just (unsafeCodePointAt0 s)
codePointAt n s = _codePointAt codePointAtFallback Just Nothing unsafeCodePointAt0 n s

foreign import _codePointAt
  :: (Int -> String -> Maybe CodePoint)
  -> (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> (String -> CodePoint)
  -> Int
  -> String
  -> Maybe CodePoint

codePointAtFallback :: Int -> String -> Maybe CodePoint
codePointAtFallback n s = case uncons s of
  Just { head, tail } -> if n == 0 then Just head else codePointAtFallback (n - 1) tail
  _ -> Nothing

-- | Returns a record with the first code point and the remaining code points
-- | of the string. Returns `Nothing` if the string is empty. Operates in
-- | constant space and time.
-- |
-- | ```purescript
-- | >>> uncons "𝐀𝐀 c 𝐀"
-- | Just { head: CodePoint 0x1D400, tail: "𝐀 c 𝐀" }
-- | >>> uncons ""
-- | Nothing
-- | ```
-- |
uncons :: String -> Maybe { head :: CodePoint, tail :: String }
uncons s = case CU.length s of
  0 -> Nothing
  1 -> Just { head: CodePoint (fromEnum (Unsafe.charAt 0 s)), tail: "" }
  _ ->
    let
      cu0 = fromEnum (Unsafe.charAt 0 s)
      cu1 = fromEnum (Unsafe.charAt 1 s)
    in
      if isLead cu0 && isTrail cu1
        then Just { head: unsurrogate cu0 cu1, tail: CU.drop 2 s }
        else Just { head: CodePoint cu0, tail: CU.drop 1 s }

-- | Returns the number of code points in the string. Operates in constant
-- | space and in time linear to the length of the string.
-- |
-- | ```purescript
-- | >>> length "b 𝐀𝐀 c 𝐀"
-- | 8
-- | -- compare to Data.String:
-- | >>> length "b 𝐀𝐀 c 𝐀"
-- | 11
-- | ```
-- |
length :: String -> Int
length = Array.length <<< toCodePointArray

-- | Returns the number of code points in the leading sequence of code points
-- | which all match the given predicate. Operates in constant space and in
-- | time linear to the length of the string.
-- |
-- | ```purescript
-- | >>> countPrefix (\c -> fromEnum c == 0x1D400) "𝐀𝐀 b c 𝐀"
-- | 2
-- | ```
-- |
countPrefix :: (CodePoint -> Boolean) -> String -> Int
countPrefix = _countPrefix countFallback unsafeCodePointAt0

foreign import _countPrefix
  :: ((CodePoint -> Boolean) -> String -> Int)
  -> (String -> CodePoint)
  -> (CodePoint -> Boolean)
  -> String
  -> Int

countFallback :: (CodePoint -> Boolean) -> String -> Int
countFallback p s = countTail p s 0

countTail :: (CodePoint -> Boolean) -> String -> Int -> Int
countTail p s accum = case uncons s of
  Just { head, tail } -> if p head then countTail p tail (accum + 1) else accum
  _ -> accum

-- | Returns the number of code points preceding the first match of the given
-- | pattern in the string. Returns `Nothing` when no matches are found.
-- |
-- | ```purescript
-- | >>> indexOf (Pattern "𝐀") "b 𝐀𝐀 c 𝐀"
-- | Just 2
-- | >>> indexOf (Pattern "o") "b 𝐀𝐀 c 𝐀"
-- | Nothing
-- | ```
-- |
indexOf :: Pattern -> String -> Maybe Int
indexOf p s = (\i -> length (CU.take i s)) <$> CU.indexOf p s

-- | Returns the number of code points preceding the first match of the given
-- | pattern in the string. Pattern matches preceding the given index will be
-- | ignored. Returns `Nothing` when no matches are found.
-- |
-- | ```purescript
-- | >>> indexOf' (Pattern "𝐀") 4 "b 𝐀𝐀 c 𝐀"
-- | Just 7
-- | >>> indexOf' (Pattern "o") 4 "b 𝐀𝐀 c 𝐀"
-- | Nothing
-- | ```
-- |
indexOf' :: Pattern -> Int -> String -> Maybe Int
indexOf' p i s =
  let s' = drop i s in
  (\k -> i + length (CU.take k s')) <$> CU.indexOf p s'

-- | Returns the number of code points preceding the last match of the given
-- | pattern in the string. Returns `Nothing` when no matches are found.
-- |
-- | ```purescript
-- | >>> lastIndexOf (Pattern "𝐀") "b 𝐀𝐀 c 𝐀"
-- | Just 7
-- | >>> lastIndexOf (Pattern "o") "b 𝐀𝐀 c 𝐀"
-- | Nothing
-- | ```
-- |
lastIndexOf :: Pattern -> String -> Maybe Int
lastIndexOf p s = (\i -> length (CU.take i s)) <$> CU.lastIndexOf p s

-- | Returns the number of code points preceding the first match of the given
-- | pattern in the string. Pattern matches following the given index will be
-- | ignored.
-- |
-- | Giving a negative index is equivalent to giving 0 and giving an index
-- | greater than the number of code points in the string is equivalent to
-- | searching in the whole string.
-- |
-- | Returns `Nothing` when no matches are found.
-- |
-- | ```purescript
-- | >>> lastIndexOf' (Pattern "𝐀") (-1) "b 𝐀𝐀 c 𝐀"
-- | Nothing
-- | >>> lastIndexOf' (Pattern "𝐀") 0 "b 𝐀𝐀 c 𝐀"
-- | Nothing
-- | >>> lastIndexOf' (Pattern "𝐀") 5 "b 𝐀𝐀 c 𝐀"
-- | Just 3
-- | >>> lastIndexOf' (Pattern "𝐀") 8 "b 𝐀𝐀 c 𝐀"
-- | Just 7
-- | >>> lastIndexOf' (Pattern "o") 5 "b 𝐀𝐀 c 𝐀"
-- | Nothing
-- | ```
-- |
lastIndexOf' :: Pattern -> Int -> String -> Maybe Int
lastIndexOf' p i s =
  let i' = CU.length (take i s) in
  (\k -> length (CU.take k s)) <$> CU.lastIndexOf' p i' s

-- | Returns a string containing the given number of code points from the
-- | beginning of the given string. If the string does not have that many code
-- | points, returns the empty string. Operates in constant space and in time
-- | linear to the given number.
-- |
-- | ```purescript
-- | >>> take 3 "b 𝐀𝐀 c 𝐀"
-- | "b 𝐀"
-- | -- compare to Data.String:
-- | >>> take 3 "b 𝐀𝐀 c 𝐀"
-- | "b �"
-- | ```
-- |
take :: Int -> String -> String
take = _take takeFallback

foreign import _take :: (Int -> String -> String) -> Int -> String -> String

takeFallback :: Int -> String -> String
takeFallback n _ | n < 1 = ""
takeFallback n s = case uncons s of
  Just { head, tail } -> singleton head <> takeFallback (n - 1) tail
  _ -> s

-- | Returns a string containing the leading sequence of code points which all
-- | match the given predicate from the string. Operates in constant space and
-- | in time linear to the length of the string.
-- |
-- | ```purescript
-- | >>> takeWhile (\c -> fromEnum c == 0x1D400) "𝐀𝐀 b c 𝐀"
-- | "𝐀𝐀"
-- | ```
-- |
takeWhile :: (CodePoint -> Boolean) -> String -> String
takeWhile p s = take (countPrefix p s) s

-- | Drops the given number of code points from the beginning of the string. If
-- | the string does not have that many code points, returns the empty string.
-- | Operates in constant space and in time linear to the given number.
-- |
-- | ```purescript
-- | >>> drop 5 "𝐀𝐀 b c"
-- | "c"
-- | -- compared to Data.String:
-- | >>> drop 5 "𝐀𝐀 b c"
-- | "b c" -- because "𝐀" occupies 2 code units
-- | ```
-- |
drop :: Int -> String -> String
drop n s = CU.drop (CU.length (take n s)) s

-- | Drops the leading sequence of code points which all match the given
-- | predicate from the string. Operates in constant space and in time linear
-- | to the length of the string.
-- |
-- | ```purescript
-- | >>> dropWhile (\c -> fromEnum c == 0x1D400) "𝐀𝐀 b c 𝐀"
-- | " b c 𝐀"
-- | ```
-- |
dropWhile :: (CodePoint -> Boolean) -> String -> String
dropWhile p s = drop (countPrefix p s) s

-- | Splits a string into two substrings, where `before` contains the code
-- | points up to (but not including) the given index, and `after` contains the
-- | rest of the string, from that index on.
-- |
-- | ```purescript
-- | >>> splitAt 3 "b 𝐀𝐀 c 𝐀"
-- | { before: "b 𝐀", after: "𝐀 c 𝐀" }
-- | ```
-- |
-- | Thus the length of `(splitAt i s).before` will equal either `i` or
-- | `length s`, if that is shorter. (Or if `i` is negative the length will be
-- | 0.)
-- |
-- | In code:
-- | ```purescript
-- | length (splitAt i s).before == min (max i 0) (length s)
-- | (splitAt i s).before <> (splitAt i s).after == s
-- | splitAt i s == {before: take i s, after: drop i s}
-- | ```
splitAt :: Int -> String -> { before :: String, after :: String }
splitAt i s =
  let before = take i s in
  { before
  -- inline drop i s to reuse the result of take i s
  , after: CU.drop (CU.length before) s
  }

unsurrogate :: Int -> Int -> CodePoint
unsurrogate lead trail = CodePoint ((lead - 0xD800) * 0x400 + (trail - 0xDC00) + 0x10000)

isLead :: Int -> Boolean
isLead cu = 0xD800 <= cu && cu <= 0xDBFF

isTrail :: Int -> Boolean
isTrail cu = 0xDC00 <= cu && cu <= 0xDFFF

fromCharCode :: Int -> String
fromCharCode = CU.singleton <<< toEnumWithDefaults bottom top

-- WARN: this function expects the String parameter to be non-empty
unsafeCodePointAt0 :: String -> CodePoint
unsafeCodePointAt0 = _unsafeCodePointAt0 unsafeCodePointAt0Fallback

foreign import _unsafeCodePointAt0
  :: (String -> CodePoint)
  -> String
  -> CodePoint

unsafeCodePointAt0Fallback :: String -> CodePoint
unsafeCodePointAt0Fallback s =
  let
    cu0 = fromEnum (Unsafe.charAt 0 s)
  in
    if isLead cu0 && CU.length s > 1
       then
         let cu1 = fromEnum (Unsafe.charAt 1 s) in
         if isTrail cu1 then unsurrogate cu0 cu1 else CodePoint cu0
       else
         CodePoint cu0
