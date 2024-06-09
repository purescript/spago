-- | Utilities to interop between different codec libraries
module Docs.Search.JsonCodec
  ( fromUni
  , inject
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ
import Codec.JSON.DecodeError as CJ.DecodeError
import Codec.Json.Unidirectional.Value (DecodeError, printDecodeError)
import Control.Monad.Except (Except, except)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, inj)
import JSON (JSON)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Equivalent to `Data.Variant.inj`, just uses a Visible Type Application instead of Proxy
-- | Useful for deriving codecs for sum types
inject :: forall @sym a r1 r2. Cons sym a r1 r2 => IsSymbol sym => a -> Variant r2
inject = inj (Proxy :: _ sym)

fromUni
  :: forall a
   . (Json -> Either DecodeError a)
  -> (JSON -> Except CJ.DecodeError a)
fromUni fn = except <<< lmap convertError <<< (fn <<< toArgonaut)
  where
  convertError :: DecodeError -> CJ.DecodeError
  convertError = printDecodeError >>> CJ.DecodeError.basic

  toArgonaut :: JSON -> Json
  toArgonaut = unsafeCoerce
