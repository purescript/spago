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
import Data.Argonaut.Core (Json)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Docs.Search.DocTypes (AnyOpName, ChildDeclaration(..), ChildDeclarationInfo(..), ClassName, Constraint(..), Constraint', ConstraintData(..), ConstructorName, DataDeclType(..), Declaration(..), DeclarationInfo(..), DocLink(..), DocModule(..), GithubRepo(..), GithubUser(..), Ident(..), InPackage(..), InternalIdentData(..), KindInfo(..), Label(..), LinkLocation(..), LinksContext(..), ManifestError(..), ModuleName(..), Name(..), Namespace, NotYetKnown(..), OpName(..), OpNameType, Package(..), PackageError(..), ProperName(..), ProperNameType, Qualified(..), QualifiedBy(..), RowListItem(..), SkolemScope(..), SourceAnn(..), SourceConstraint, SourcePos(..), SourceSpan(..), SourceType, Type(..), Type', TypeName, TypeOpName, TypeVarVisibility(..), UploadedPackage, ValueOpName, VerifiedPackage, WildcardData(..), _ss, _sss, byNullSourcePos, compareConstraint, compareMaybeType, compareType, eqConstraint, eqMaybeType, eqType, fromChildDeclaration, fromChildDeclarationInfo, fromConstraint, fromConstraintData, fromDataDeclType, fromDeclaration, fromDeclarationInfo, fromDocModule, fromGithubRepo, fromGithubUser, fromISO8601, fromIdent, fromInPackage, fromInternalIdentData, fromKindInfo, fromLabel, fromModuleName, fromNotYetKnown, fromOpName, fromPackage, fromProperName, fromQualified, fromQualifiedBy, fromSkolemScope, fromSourceAnn, fromSourcePos, fromSourceSpan, fromType, fromTypeVarVisibility, fromVersion, fromWildcardData, hh_mm, kindType, nullSourceAnn, nullSourcePos, nullSourceSpan, showDataDeclType, srcTypeConstructor, toAsConstrantUnit, toChildDeclaration, toChildDeclarationInfo, toConstraint', toConstraintData, toConstraintUnit, toDataDeclType, toDeclaration, toDeclarationInfo, toDocModule, toFunDeps, toGithubRepo, toGithubUser, toISO8601, toIdent, toInPackage, toInternalIdentData, toKindInfo, toLabel, toModuleName, toNotYetKnown, toOpName, toPackage, toProperName, toQualified, toQualifiedBy, toSkolemScope, toSourceAnn, toSourceConstraint, toSourcePos, toSourceSpan, toSourceType, toType', toTypeArguments, toTypeUnit, toTypeVarVisibility, toUploadedPackage, toVersion, toWildcardData, yyyy_mm_dd) as ReExport
import Docs.Search.DocTypes (Constraint, DataDeclType, ProperName, Qualified, Type')
import Docs.Search.DocTypes as DocTypes
import Docs.Search.JsonCodec as JsonCodec
import JSON (JSON)
import JSON as JSON
import Unsafe.Coerce (unsafeCoerce)

type QualifiedName tag = Qualified (ProperName tag)

qualifiedNameCodec :: forall tag. CJ.Codec (QualifiedName tag)
qualifiedNameCodec = Codec.codec'
  (JsonCodec.fromUni $ DocTypes.toQualified DocTypes.toProperName)
  (fromArgonaut <<< DocTypes.fromQualified DocTypes.fromProperName)

type FunDeps = Array FunDep
type FunDep = Tuple (Array String) (Array String)

funDepsCodec :: CJ.Codec FunDeps
funDepsCodec = CJ.array $ CJ.Common.tuple typeVarsCodec typeVarsCodec
  where
  typeVarsCodec = CJ.array CJ.string

type TypeArgument =
  { name :: String
  , kind :: Maybe Type'
  }

typeArgumentCodec :: CJ.Codec TypeArgument
typeArgumentCodec = CJ.named "TypeArgument" $
  CJ.Record.object
    { name: CJ.string
    , kind: CJ.Record.optional typeCodec
    }

dataDeclTypeCodec :: CJ.Codec DataDeclType
dataDeclTypeCodec =
  Codec.codec'
    (JsonCodec.fromUni DocTypes.toDataDeclType)
    (fromArgonaut <<< DocTypes.fromDataDeclType)

typeCodec :: CJ.Codec Type'
typeCodec =
  Codec.codec'
    (JsonCodec.fromUni DocTypes.toTypeUnit)
    (fromArgonaut <<< DocTypes.fromType \_ -> toArgonaut JSON.null)

type Constraint' = Constraint Unit

constraintCodec :: CJ.Codec Constraint'
constraintCodec =
  Codec.codec'
    (JsonCodec.fromUni $ DocTypes.toConstraintUnit Json.toJNull)
    (fromArgonaut <<< DocTypes.fromConstraint \_ -> toArgonaut JSON.null)

fromArgonaut :: Json -> JSON
fromArgonaut = unsafeCoerce

toArgonaut :: JSON -> Json
toArgonaut = unsafeCoerce
