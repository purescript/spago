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

import Prelude
import Prim hiding (Constraint)

import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core as Argonaut
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Docs.Search.DocTypes (AnyOpName, ChildDeclaration(..), ChildDeclarationInfo(..), ClassName, Constraint(..), Constraint', ConstraintData(..), ConstructorName, DataDeclType(..), Declaration(..), DeclarationInfo(..), DocLink(..), DocModule(..), GithubRepo(..), GithubUser(..), Ident(..), InPackage(..), InternalIdentData(..), KindInfo(..), Label(..), LinkLocation(..), LinksContext(..), ManifestError(..), ModuleName(..), Name(..), Namespace, NotYetKnown(..), OpName(..), OpNameType, Package(..), PackageError(..), ProperName(..), ProperNameType, Qualified(..), QualifiedBy(..), RowListItem(..), SkolemScope(..), SourceAnn(..), SourceConstraint, SourcePos(..), SourceSpan(..), SourceType, Type(..), Type', TypeName, TypeOpName, TypeVarVisibility(..), UploadedPackage, ValueOpName, VerifiedPackage, WildcardData(..), _ss, _sss, byNullSourcePos, compareConstraint, compareMaybeType, compareType, eqConstraint, eqMaybeType, eqType, fromChildDeclaration, fromChildDeclarationInfo, fromConstraint, fromConstraintData, fromDataDeclType, fromDeclaration, fromDeclarationInfo, fromDocModule, fromGithubRepo, fromGithubUser, fromISO8601, fromIdent, fromInPackage, fromInternalIdentData, fromKindInfo, fromLabel, fromModuleName, fromNotYetKnown, fromOpName, fromPackage, fromProperName, fromQualified, fromQualifiedBy, fromSkolemScope, fromSourceAnn, fromSourcePos, fromSourceSpan, fromType, fromTypeVarVisibility, fromVersion, fromWildcardData, hh_mm, kindType, nullSourceAnn, nullSourcePos, nullSourceSpan, showDataDeclType, srcTypeConstructor, toAsConstrantUnit, toChildDeclaration, toChildDeclarationInfo, toConstraint', toConstraintData, toConstraintUnit, toDataDeclType, toDeclaration, toDeclarationInfo, toDocModule, toFunDeps, toGithubRepo, toGithubUser, toISO8601, toIdent, toInPackage, toInternalIdentData, toKindInfo, toLabel, toModuleName, toNotYetKnown, toOpName, toPackage, toProperName, toQualified, toQualifiedBy, toSkolemScope, toSourceAnn, toSourceConstraint, toSourcePos, toSourceSpan, toSourceType, toType', toTypeArguments, toTypeUnit, toTypeVarVisibility, toUploadedPackage, toVersion, toWildcardData, yyyy_mm_dd) as ReExport
import Docs.Search.DocTypes (Constraint, DataDeclType, ProperName, Qualified, Type')
import Docs.Search.DocTypes as DocTypes
import Docs.Search.JsonCodec as JsonCodec

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
