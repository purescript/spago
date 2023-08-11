module Data.Binary.Base64
  ( decode
  , encode
  , encodeUrl
  )
where

import Data.ArrayBuffer.Types      ( Uint8Array )
import Data.Either                 ( Either (Left, Right) )
import Data.Function.Uncurried     ( Fn1, Fn3, runFn1, runFn3 )
import Data.String.Base64          ( atob, btoa )
import Data.String.Base64.Internal ( atobIsDefined
                                   , btoaIsDefined
                                   , uint8ArrayToBtoaSafeString
                                   , unsafeStringToUint8ArrayOfCharCodes
                                   , toRfc4648
                                   , toUrlSafe
                                   , unsafeFromRight
                                   )
import Effect.Exception            ( Error )
import Prelude


-- | Encode a `Uint8Array` to its (RFC 4648) Base64 representation.
-- |
-- | Example:
-- | ```purescript
-- | encode $ encodeUtf8 "柿くへば鐘が鳴るなり法隆寺"
-- | -- "5p+/44GP44G444Gw6ZCY44GM6bO044KL44Gq44KK5rOV6ZqG5a+6"
-- | ```
encode :: Uint8Array -> String
encode uInt8Array =
  if btoaIsDefined
    then
      unsafeFromRight (btoa <<< uint8ArrayToBtoaSafeString $ uInt8Array)
    else
      encodeNode uInt8Array

encodeNode :: Uint8Array -> String
encodeNode u8 = runFn1 encodeNodeImpl u8

foreign import encodeNodeImpl :: Fn1 Uint8Array String

-- | Encode a `Uint8Array` to a URL-safe Base64 representation.
-- |
-- | Example:
-- | ```purescript
-- | encodeUrl $ encodeUtf8 "柿くへば鐘が鳴るなり法隆寺"
-- | -- "5p-_44GP44G444Gw6ZCY44GM6bO044KL44Gq44KK5rOV6ZqG5a-6"
-- | ```
encodeUrl :: Uint8Array -> String
encodeUrl = toUrlSafe <<< encode

-- | Decode a Base64-encoded `String`.
-- | This function handles both normal (`RFC 4648`) and URL-safe input strings.
-- | Returns an `Error` for invalid input strings.
-- |
-- | Example:
-- | ```purescript
-- | decode "5p+/44GP44G444Gw6ZCY44GM6bO044KL44Gq44KK5rOV6ZqG5a+6"
-- |   >>= decodeUtf8
-- | -- Right "柿くへば鐘が鳴るなり法隆寺"
-- |
-- | decode "5p-_44GP44G444Gw6ZCY44GM6bO044KL44Gq44KK5rOV6ZqG5a-6"
-- |   >>= decodeUtf8
-- | -- Right "柿くへば鐘が鳴るなり法隆寺"
-- |
-- | decode "∀"
-- | -- ✗ Invalid input string
-- | ```
decode :: String -> Either Error Uint8Array
decode str =
  if atobIsDefined
    then
      unsafeStringToUint8ArrayOfCharCodes <$> atob (toRfc4648 str)
    else
      runFn3 decodeNodeImpl Left Right (toRfc4648 str)

foreign import decodeNodeImpl :: Fn3
  (∀ x y. x -> Either x y)
  (∀ x y. y -> Either x y)
  String
  (Either Error Uint8Array)
