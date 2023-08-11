module Data.String.NonEmpty.CodePoints
  ( fromCodePointArray
  , fromNonEmptyCodePointArray
  , singleton
  , cons
  , snoc
  , fromFoldable1
  , toCodePointArray
  , toNonEmptyCodePointArray
  , codePointAt
  , indexOf
  , indexOf'
  , lastIndexOf
  , lastIndexOf'
  , uncons
  , length
  , take
  -- takeRight
  , takeWhile
  , drop
  -- dropRight
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
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CP
import Data.String.NonEmpty.Internal (NonEmptyString(..), fromString)
import Data.String.Pattern (Pattern)
import Partial.Unsafe (unsafePartial)

-- For internal use only. Do not export.
toNonEmptyString :: String -> NonEmptyString
toNonEmptyString = NonEmptyString

-- For internal use only. Do not export.
fromNonEmptyString :: NonEmptyString -> String
fromNonEmptyString (NonEmptyString s) = s

-- For internal use only. Do not export.
liftS :: forall r. (String -> r) -> NonEmptyString -> r
liftS f (NonEmptyString s) = f s

fromCodePointArray :: Array CodePoint -> Maybe NonEmptyString
fromCodePointArray = case _ of
  [] -> Nothing
  cs -> Just (toNonEmptyString (CP.fromCodePointArray cs))

fromNonEmptyCodePointArray :: NonEmptyArray CodePoint -> NonEmptyString
fromNonEmptyCodePointArray = unsafePartial fromJust <<< fromCodePointArray <<< NEA.toArray

singleton :: CodePoint -> NonEmptyString
singleton = toNonEmptyString <<< CP.singleton

cons :: CodePoint -> String -> NonEmptyString
cons c s = toNonEmptyString (CP.singleton c <> s)

snoc :: CodePoint -> String -> NonEmptyString
snoc c s = toNonEmptyString (s <> CP.singleton c)

fromFoldable1 :: forall f. Foldable1 f => f CodePoint -> NonEmptyString
fromFoldable1 = F1.foldMap1 singleton

toCodePointArray :: NonEmptyString -> Array CodePoint
toCodePointArray = CP.toCodePointArray <<< fromNonEmptyString

toNonEmptyCodePointArray :: NonEmptyString -> NonEmptyArray CodePoint
toNonEmptyCodePointArray = unsafePartial fromJust <<< NEA.fromArray <<< toCodePointArray

codePointAt :: Int -> NonEmptyString -> Maybe CodePoint
codePointAt = liftS <<< CP.codePointAt

indexOf :: Pattern -> NonEmptyString -> Maybe Int
indexOf = liftS <<< CP.indexOf

indexOf' :: Pattern -> Int -> NonEmptyString -> Maybe Int
indexOf' pat = liftS <<< CP.indexOf' pat

lastIndexOf :: Pattern -> NonEmptyString -> Maybe Int
lastIndexOf = liftS <<< CP.lastIndexOf

lastIndexOf' :: Pattern -> Int -> NonEmptyString -> Maybe Int
lastIndexOf' pat = liftS <<< CP.lastIndexOf' pat

uncons :: NonEmptyString -> { head :: CodePoint, tail :: Maybe NonEmptyString }
uncons nes =
  let
    s = fromNonEmptyString nes
  in
    { head: unsafePartial fromJust (CP.codePointAt 0 s)
    , tail: fromString (CP.drop 1 s)
    }

length :: NonEmptyString -> Int
length = CP.length <<< fromNonEmptyString

take :: Int -> NonEmptyString -> Maybe NonEmptyString
take i nes =
  let
    s = fromNonEmptyString nes
  in
    if i < 1
      then Nothing
      else Just (toNonEmptyString (CP.take i s))

takeWhile :: (CodePoint -> Boolean) -> NonEmptyString -> Maybe NonEmptyString
takeWhile f = fromString <<< liftS (CP.takeWhile f)

drop :: Int -> NonEmptyString -> Maybe NonEmptyString
drop i nes =
  let
    s = fromNonEmptyString nes
  in
    if i >= CP.length s
      then Nothing
      else Just (toNonEmptyString (CP.drop i s))

dropWhile :: (CodePoint -> Boolean) -> NonEmptyString -> Maybe NonEmptyString
dropWhile f = fromString <<< liftS (CP.dropWhile f)

countPrefix :: (CodePoint -> Boolean) -> NonEmptyString -> Int
countPrefix = liftS <<< CP.countPrefix

splitAt
  :: Int
  -> NonEmptyString
  -> { before :: Maybe NonEmptyString, after :: Maybe NonEmptyString }
splitAt i nes =
  case CP.splitAt i (fromNonEmptyString nes) of
    { before, after } -> { before: fromString before, after: fromString after }
