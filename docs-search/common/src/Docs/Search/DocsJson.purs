-- | A module containing everything that is necessary to decode `docs.json` files.
module Docs.Search.DocsJson
  ( module ReExport
  , sourceSpanCodec
  ) where

import Docs.Search.TypeDecoder

import Prelude
import Prim hiding (Type, Constraint)

import Data.Argonaut.Core (fromString, stringify, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (wrapIso)
import Data.Tuple (Tuple)

import Language.PureScript.AST.SourcePos as ReExport
import Language.PureScript.Docs.Types as ReExport
import Language.PureScript.Environment as ReExport
import Language.PureScript.AST.SourcePos (SourceSpan(..)) as ReExport
import Language.PureScript.Environment (DataDeclType(..))
import Language.PureScript.AST.SourcePos (SourceSpan(..), SourcePos(..))

sourceSpanCodec :: JsonCodec SourceSpan
sourceSpanCodec =
  wrapIso SourceSpan $
    CAR.object "SourceSpan"
      { start: sourcePosCodec
      , end: sourcePosCodec
      , name: CA.string
      }

sourcePosCodec :: JsonCodec SourcePos
sourcePosCodec =
  wrapIso SourcePos $
    CAR.object "SourcePos"
      { line: CA.int
      , column: CA.int
      }

{-
instance EncodeJson DataDeclType where
  encodeJson = fromString
    <<< case _ of
      Newtype -> "newtype"
      Data -> "data"
    <<< unwrap

instance DecodeJson DataDeclType where
  decodeJson json =
    case toString json of
      Just tag ->
        case tag of
          "newtype" -> Right $ wrap Newtype
          "data" -> Right $ wrap Data
          _ -> errorWith $ "Couldn't decode DataDeclType: " <> tag
      Nothing -> errorWith $ "Couldn't decode DataDeclType: " <> stringify json
  where
  errorWith :: forall a. String -> Either JsonDecodeError a
  errorWith = Left <<< TypeMismatch
-}
