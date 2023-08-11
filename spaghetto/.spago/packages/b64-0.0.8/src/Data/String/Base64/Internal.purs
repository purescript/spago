module Data.String.Base64.Internal
  ( atobIsDefined
  , btoaIsDefined
  , uint8ArrayToBtoaSafeString
  , unsafeFromRight
  , unsafeStringToUint8ArrayOfCharCodes
  , toUrlSafe
  , toRfc4648
  )
where

import Data.ArrayBuffer.Types  ( Uint8Array )
import Data.Either             ( Either, either )
import Data.Enum               ( fromEnum )
import Data.Function.Uncurried ( Fn1, runFn1 )
import Data.String             ( Pattern(Pattern), Replacement(Replacement)
                               , replaceAll, toCodePointArray
                               )
import Partial.Unsafe          ( unsafeCrashWith )
import Prelude


foreign import atobIsDefined :: Boolean
foreign import btoaIsDefined :: Boolean

uint8ArrayToBtoaSafeString :: Uint8Array -> String
uint8ArrayToBtoaSafeString u8 = runFn1 uint8ArrayToBtoaSafeStringImpl u8

foreign import uint8ArrayToBtoaSafeStringImpl :: Fn1 Uint8Array String

asUint8Array :: Array Int -> Uint8Array
asUint8Array arr = runFn1 asUint8ArrayImpl arr

foreign import asUint8ArrayImpl :: Fn1 (Array Int) Uint8Array

-- Helper function to convert (a very specific set of) strings to a `Uint8Array`
-- of Unicode code points.
-- This function is only meant to be used on output strings of the `atob`
-- function. It will NOT work correctly on strings that contain characters
-- whose Unicode code points are outside the range 0 .. U+00FF.
unsafeStringToUint8ArrayOfCharCodes :: String -> Uint8Array
unsafeStringToUint8ArrayOfCharCodes =
  asUint8Array <<< map fromEnum <<< toCodePointArray


-- RFC 4648/URL-safe alphabet conversion
--
-- A commonly used variant of Base64 encodes to URL-safe strings and thus
-- uses a slightly different alphabet:
-- ```
-- RFC 4648: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
-- URL-safe: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
-- ```
--
-- Note that URL-safe encoding also removes any `=` padding.
-- The following functions help to convert between the two alphabets.

toUrlSafe :: String -> String
toUrlSafe = replaceAll (Pattern "=") (Replacement "")
        <<< replaceAll (Pattern "/") (Replacement "_")
        <<< replaceAll (Pattern "+") (Replacement "-")

toRfc4648 :: String -> String
toRfc4648 = replaceAll (Pattern "-") (Replacement "+")
        <<< replaceAll (Pattern "_") (Replacement "/")


-- Only use this function when you are absolutely sure that the argument is a
-- `Right` value.
unsafeFromRight :: ∀ a b. Either a b -> b
unsafeFromRight = either (\_ -> unsafeCrashWith crashMessage) identity
  where
    crashMessage = "This should never happen! If you see this message, please \
                   \file a bug report in the `purescript-b64` issue tracker."
