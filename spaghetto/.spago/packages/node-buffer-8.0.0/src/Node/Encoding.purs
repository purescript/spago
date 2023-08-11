module Node.Encoding
  ( Encoding (..)
  , encodingToNode
  , byteLength
  ) where

import Prelude

data Encoding
  = ASCII
  | UTF8
  | UTF16LE
  | UCS2
  | Base64
  | Latin1
  | Binary
  | Hex

instance showEncoding :: Show Encoding where
  show ASCII   = "ASCII"
  show UTF8    = "UTF8"
  show UTF16LE = "UTF16LE"
  show UCS2    = "UCS2"
  show Base64  = "Base64"
  show Latin1  = "Latin1"
  show Binary  = "Binary"
  show Hex     = "Hex"

-- | Convert an `Encoding` to a `String` in the format expected by Node.js
-- | APIs.
encodingToNode :: Encoding -> String
encodingToNode ASCII   = "ascii"
encodingToNode UTF8    = "utf8"
encodingToNode UTF16LE = "utf16le"
encodingToNode UCS2    = "ucs2"
encodingToNode Base64  = "base64"
encodingToNode Latin1  = "latin1"
encodingToNode Binary  = "binary"
encodingToNode Hex     = "hex"

foreign import byteLengthImpl :: String -> String -> Int

byteLength :: String -> Encoding -> Int
byteLength str enc = byteLengthImpl str (encodingToNode enc)
