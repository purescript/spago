module Docs.Search.TypeDecoder
  ( module ReExport
  , dataDeclTypeCodec
  , typeCodec
  , TypeArgument
  , typeArgumentCodec
  , FunDeps
  , funDepsCodec
  , FunDep
  , QualifiedName
  , qualifiedNameCodec
  , constraintCodec
  ) where

import Docs.Search.DocTypes as ReExport

import Docs.Search.Types (Identifier)
import Docs.Search.DocTypes (Type', Type(..), Constraint(..), Label(..), DataDeclType, ProperName(..), Qualified(..))
import Docs.Search.DocTypes as DocTypes
import Docs.Search.JsonCodec as JsonCodec

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core as Argonaut
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Record as CAR
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple)

type QualifiedName tag = Qualified (ProperName tag)

qualifiedNameCodec :: forall tag. CA.JsonCodec (QualifiedName tag)
qualifiedNameCodec = CA.codec'
  (JsonCodec.fromUni $ DocTypes.toQualified DocTypes.toProperName)
  (DocTypes.fromQualified DocTypes.fromProperName)

type FunDeps = Array FunDep
type FunDep = Tuple (Array String) (Array String)

funDepsCodec :: CA.JsonCodec FunDeps
funDepsCodec = CA.array $ CA.tuple typeVarsCodec typeVarsCodec
  where
  typeVarsCodec = CA.array CA.string

type TypeArgument =
  { name :: String
  , kind :: Maybe Type'
  }

typeArgumentCodec :: CA.JsonCodec TypeArgument
typeArgumentCodec =
  CAR.object "TypeArgument"
    { name: CA.string
    , kind: CAR.optional typeCodec
    }

dataDeclTypeCodec :: CA.JsonCodec DataDeclType
dataDeclTypeCodec =
  CA.codec'
    (JsonCodec.fromUni $ DocTypes.toDataDeclType)
    (DocTypes.fromDataDeclType)

typeCodec :: CA.JsonCodec Type'
typeCodec =
  CA.codec'
    (JsonCodec.fromUni $ DocTypes.toTypeUnit)
    (DocTypes.fromType $ const Argonaut.jsonNull)

type Constraint' = Constraint Unit

constraintCodec :: CA.JsonCodec Constraint'
constraintCodec =
  CA.codec'
    (JsonCodec.fromUni $ DocTypes.toConstraintUnit Json.toJNull)
    (DocTypes.fromConstraint $ const Argonaut.jsonNull)
