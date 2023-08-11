module Data.String.Base64
  ( atob
  , btoa
  , decode
  , encode
  , encodeUrl
  )
where

import Data.Either                 ( Either (Left, Right) )
import Data.Function.Uncurried     ( Fn1, Fn3, runFn1, runFn3 )
import Data.String.Base64.Internal ( atobIsDefined
                                   , btoaIsDefined
                                   , uint8ArrayToBtoaSafeString
                                   , unsafeFromRight
                                   , unsafeStringToUint8ArrayOfCharCodes
                                   , toUrlSafe
                                   , toRfc4648
                                   )
import Data.TextDecoder            ( decodeUtf8 )
import Data.TextEncoder            ( encodeUtf8 )
import Effect.Exception            ( Error )
import Prelude


-- | Encode a `String` to its (RFC 4648) Base64 representation.
-- |
-- | Example:
-- | ```purescript
-- | encode "柿くへば鐘が鳴るなり法隆寺"
-- | -- "5p+/44GP44G444Gw6ZCY44GM6bO044KL44Gq44KK5rOV6ZqG5a+6"
-- | ```
{-
  Basically this works as follows:
  Suppose you have a string: "∀ a"
  The following table shows the individual characters, their code points
  and their UTF-8 representation.

  Char   CP (Dec)   CP (Hex)   UTF-8 (Hex)   UTF-8 (Bin)
  ---------------------------------------------------------------------
  ∀      8704       u+2200     E2 88 80      11100010 10001000 10000000
  <SP>   32         u+0020     20            00100000
  a      97         u+0061     61            01100001
  ---------------------------------------------------------------------

  In a first step, the string is converted to its UTF-8 representation.
  After that, each individual byte is mapped to a (btoa-safe) character.

  Representation   |------------ ∀ -------------|     <SP>        a
  ---------------------------------------------------------------------
  Bin              11100010   10001000   10000000   00100000   01100001
  Dec              226        136        128        32         97
  Btoa-safe char   â          <HTS>      <PAD>      SP         a
  ---------------------------------------------------------------------

  Note that <HTS> and <PAD> are non-printable characters.

  The resulting string "â<HTS><PAD><SP>a" can now safely be passed to the
  `btoa` function and results in "4oiAIGE=".
-}
encode :: String -> String
encode str =
  if btoaIsDefined
    then
      unsafeFromRight (btoa <<< uint8ArrayToBtoaSafeString <<< encodeUtf8 $ str)
    else
      encodeNode str

encodeNode :: String -> String
encodeNode s = runFn1 encodeNodeImpl s

foreign import encodeNodeImpl :: Fn1 String String

-- | Encode a `String` to a URL-safe Base64 representation.
-- |
-- | Example:
-- | ```purescript
-- | encodeUrl "柿くへば鐘が鳴るなり法隆寺"
-- | -- "5p-_44GP44G444Gw6ZCY44GM6bO044KL44Gq44KK5rOV6ZqG5a-6"
-- | ```
encodeUrl :: String -> String
encodeUrl = toUrlSafe <<< encode

-- | Create a Base64-encoded ASCII `String` from a `String`.
-- | Each character in the input string is treated as one byte of binary data,
-- | hence this function returns an `Error` if a character's code point is
-- | outside the range 0x00 .. 0xFF.
-- |
-- | Example:
-- | ```purescript
-- | btoa "PureScript rocks!"
-- | -- Right "UHVyZVNjcmlwdCByb2NrcyE="
-- |
-- | btoa "∀"
-- | -- ✗ Invalid input string
-- | ```
btoa :: String -> Either Error String
btoa str = runFn3 btoaImpl Left Right str

foreign import btoaImpl :: Fn3
  (∀ x y. x -> Either x y)
  (∀ x y. y -> Either x y)
  String
  (Either Error String)

-- | Decode a Base64-encoded `String`.
-- | This function handles both normal (`RFC 4648`) and URL-safe input strings.
-- | Returns an `Error` for invalid input strings.
-- |
-- | Example:
-- | ```purescript
-- | decode "5p+/44GP44G444Gw6ZCY44GM6bO044KL44Gq44KK5rOV6ZqG5a+6"
-- | -- Right "柿くへば鐘が鳴るなり法隆寺"
-- |
-- | decode "5p-_44GP44G444Gw6ZCY44GM6bO044KL44Gq44KK5rOV6ZqG5a-6"
-- | -- Right "柿くへば鐘が鳴るなり法隆寺"
-- |
-- | decode "∀"
-- | -- ✗ Invalid input string
-- | ```
decode :: String -> Either Error String
decode str =
  if atobIsDefined
    then
      unsafeStringToUint8ArrayOfCharCodes <$> atob (toRfc4648 str)
        >>= decodeUtf8
    else
      runFn3 decodeNodeImpl Left Right (toRfc4648 str)

foreign import decodeNodeImpl :: Fn3
  (∀ x y. x -> Either x y)
  (∀ x y. y -> Either x y)
  String
  (Either Error String)

-- | Decode a Base64-encoded `String` via the native `atob` function.
-- | Returns an `Error` for malformed input strings.
-- |
-- | Example:
-- | ```purescript
-- | atob "UHVyZVNjcmlwdCByb2NrcyE="
-- | Right "PureScript rocks!"
-- |
-- | atob "∀"
-- | -- ✗ Invalid input string
-- | ```
atob :: String -> Either Error String
atob str = runFn3 atobImpl Left Right str

foreign import atobImpl :: Fn3
  (∀ x y. x -> Either x y)
  (∀ x y. y -> Either x y)
  String
  (Either Error String)
