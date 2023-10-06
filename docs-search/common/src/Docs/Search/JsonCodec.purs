-- | Utilities to interop between different codec libraries
module Docs.Search.JsonCodec
  ( fromGeneric
  , fromJsonUnidirectional
  , inject
  ) where

import Prelude
import Prim.Row (class Cons)

import Codec.Json.Unidirectional.Value (DecodeError, printDecodeError)
import Data.Argonaut.Core (Json, fromString, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error as Generic
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Sum as CAS
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Profunctor.Choice (left)
import Data.Symbol (class IsSymbol)
import Data.Variant
import Type.Proxy (Proxy(..))

-- | Equivalent to `Data.Variant.inj`, just uses a Visible Type Application instead of Proxy
-- | Useful for deriving codecs for sum types
inject :: forall @sym a r1 r2. Cons sym a r1 r2 => IsSymbol sym => a -> Variant r2
inject = inj (Proxy :: _ sym)

fromJsonUnidirectional
  :: forall a
   . (Json -> Either DecodeError a)
  -> (Json -> Either JsonDecodeError a)
fromJsonUnidirectional = map $ left convertError
  where
  convertError :: DecodeError -> JsonDecodeError
  convertError = printDecodeError >>> TypeMismatch

-- TODO: Delete this after done migrating to the codecs in `language-purescript`
-- currently still around for the sake of the types in these modules:
-- Docs.Search.DocsJson
-- Docs.Search.TypeDecoder
-- | Create a codec from instances of EncodeJson & DecodeJson
fromGeneric :: forall a. EncodeJson a => DecodeJson a => JsonCodec a
fromGeneric =
  CA.codec'
    (left convertError <<< decodeJson)
    encodeJson
  where
  convertError = Generic.printJsonDecodeError >>> TypeMismatch
