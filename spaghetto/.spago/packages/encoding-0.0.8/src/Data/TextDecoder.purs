module Data.TextDecoder
  ( Encoding (..)
  , decode
  , decodeUtf8
  )
where

import Effect.Exception        ( Error )
import Data.ArrayBuffer.Types  ( ArrayView )
import Data.Either             ( Either (Left, Right) )
import Data.Function.Uncurried ( Fn4, runFn4 )
import Prelude


-- | Decodes an `ArrayBufferView` with the given `Encoding`.
-- | Returns an `Error` if decoding fails.
decode :: ∀ a. Encoding -> (ArrayView a) -> Either Error String
decode encoding buffer = runFn4 decodeImpl Left Right (show encoding) buffer

foreign import decodeImpl :: ∀ a. Fn4
  (∀ x y. x -> Either x y)
  (∀ x y. y -> Either x y)
  String
  (ArrayView a)
  (Either Error String)

-- | Decodes a UTF-8 encoded typed array to a (UTF-16) `String`.
-- | Returns an `Error` if decoding fails.
-- | This function is provided as a convenience as UTF-8 is the
-- | encoding you will probably be using most of the time.
decodeUtf8 :: ∀ a. (ArrayView a) -> Either Error String
decodeUtf8 = decode Utf8

-- | Possible character encodings.
-- | For further information see
-- | https://encoding.spec.whatwg.org/#names-and-labels
data Encoding
  = Utf8
  -- Legacy single-byte encodings
  | Ibm866
  | Iso_8859_2
  | Iso_8859_3
  | Iso_8859_4
  | Iso_8859_5
  | Iso_8859_6
  | Iso_8859_7
  | Iso_8859_8
  | Iso_8859_8_I
  | Iso_8859_10
  | Iso_8859_13
  | Iso_8859_14
  | Iso_8859_15
  | Iso_8859_16
  | Koi8_R
  | Koi8_U
  | Macintosh
  | Windows_874
  | Windows_1250
  | Windows_1251
  | Windows_1252
  | Windows_1253
  | Windows_1254
  | Windows_1255
  | Windows_1256
  | Windows_1257
  | Windows_1258
  | X_Mac_Cyrillic
  -- Legacy multi-byte Chinese (simplified) encodings
  | Gbk
  | Gb18030
  -- Legacy multi-byte Chinese (traditional) encodings
  | Big5
  -- Legacy multi-byte Japanese encodings
  | Euc_Jp
  | Iso_2022_Jp
  | Shift_Jis
  -- Legacy multi-byte Korean encodings
  | Euc_Kr
  -- Legacy miscellaneous encodings
  | Replacement
  | Utf_16Be
  | Utf_16Le
  | X_User_Defined

-- The show instance is used to convert an `Encoding` to a suitable
-- `utfLabel` string that is used in the internal `decodeImpl` helper function.
instance showEncoding :: Show Encoding where
  show Utf8           = "utf-8"
  show Ibm866         = "ibm866"
  show Iso_8859_2     = "iso-8859-2"
  show Iso_8859_3     = "iso-8859-3"
  show Iso_8859_4     = "iso-8859-4"
  show Iso_8859_5     = "iso-8859-5"
  show Iso_8859_6     = "iso-8859-6"
  show Iso_8859_7     = "iso-8859-7"
  show Iso_8859_8     = "iso-8859-8"
  show Iso_8859_8_I   = "iso-8859-8-i"
  show Iso_8859_10    = "iso-8859-10"
  show Iso_8859_13    = "iso-8859-13"
  show Iso_8859_14    = "iso-8859-14"
  show Iso_8859_15    = "iso-8859-15"
  show Iso_8859_16    = "iso-8859-16"
  show Koi8_R         = "koi8-r"
  show Koi8_U         = "koi8-u"
  show Macintosh      = "macintosh"
  show Windows_874    = "windows-874"
  show Windows_1250   = "windows-1250"
  show Windows_1251   = "windows-1251"
  show Windows_1252   = "windows-1252"
  show Windows_1253   = "windows-1253"
  show Windows_1254   = "windows-1254"
  show Windows_1255   = "windows-1255"
  show Windows_1256   = "windows-1256"
  show Windows_1257   = "windows-1257"
  show Windows_1258   = "windows-1258"
  show X_Mac_Cyrillic = "x-max-cyrillic"
  show Gbk            = "gbk"
  show Gb18030        = "gb18030"
  show Big5           = "big5"
  show Euc_Jp         = "euc-jp"
  show Iso_2022_Jp    = "iso-2022-jp"
  show Shift_Jis      = "shift-jis"
  show Euc_Kr         = "euc-kr"
  show Replacement    = "iso-2022-kr"
  show Utf_16Be       = "utf-16be"
  show Utf_16Le       = "utf-16le"
  show X_User_Defined = "x-user-defined"
