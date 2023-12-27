-- | Utilities to interop between different codec libraries
module Docs.Search.JsonCodec
  ( fromUni
  , inject
  ) where

import Prelude

import Prim.Row (class Cons)
import Codec.Json.Unidirectional.Value (DecodeError, printDecodeError)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonDecodeError(..))
import Data.Either (Either)
import Data.Profunctor.Choice (left)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, inj)
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
