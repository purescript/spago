module Data.TextEncoder
  ( Encoding(..)
  , encode
  , encodeUtf8
  )
where

import Data.ArrayBuffer.Types  (Uint8Array)
import Data.Function.Uncurried (Fn2, runFn2)
import Prelude


-- | Encodes a `String` to a `Uint8Array` with the given `Encoding`.
encode :: Encoding -> String -> Uint8Array
encode encoding str = runFn2 encodeImpl (show encoding) str

foreign import encodeImpl :: Fn2 String String Uint8Array

-- | Encodes a `String` to a `Uint8Array` using UTF-8 encoding.
-- | This function is provided as a convenience as UTF-8 is the
-- | encoding you will probably be using most of the time.
encodeUtf8 :: String -> Uint8Array
encodeUtf8 = encode Utf8

-- | Possible character encodings.
-- | For further information see
-- | https://encoding.spec.whatwg.org/#names-and-labels
data Encoding
  = Utf8
  -- Legacy encodings
  | Utf_16Be
  | Utf_16Le

-- The show instance is used to convert an `Encoding` to a suitable
-- `utfLabel` string that is used in the internal `encodeImpl` helper function.
instance showEncoding :: Show Encoding where
  show Utf8     = "utf-8"
  show Utf_16Be = "utf-16be"
  show Utf_16Le = "utf-16le"
