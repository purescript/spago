-- | Utilities to interop between different codec libraries
module Docs.Search.JsonCodec
  ( fromUni
  , inject
  ) where

import Prelude
import Prim.Row (class Cons)

import Codec.Json.Unidirectional.Value (DecodeError, printDecodeError)
import Data.Argonaut.Core (Json, fromString, stringify)
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

fromUni
  :: forall a
   . (Json -> Either DecodeError a)
  -> (Json -> Either JsonDecodeError a)

fromUni = map $ left convertError
  where
  convertError :: DecodeError -> JsonDecodeError
  convertError = printDecodeError >>> TypeMismatch
