module Docs.Search.SearchResult where

import Docs.Search.JsonCodec (inject)
import Docs.Search.DocsJson (DataDeclType, SourceSpan, sourceSpanCodec)
import Docs.Search.TypeDecoder (Constraint, FunDeps, QualifiedName, Type, TypeArgument)
import Docs.Search.Types (Identifier(..), ModuleName, PackageInfo, PackageScore)
import Docs.Search.Types as Package
import Docs.Search.TypeDecoder (Constraint', Qualified, Type, TypeArgument, FunDeps, Type', ProperName(..), ClassName(..))
import Docs.Search.TypeDecoder as TypeDecoder
import Docs.Search.JsonCodec as JsonCodec
import Docs.Search.DocsJson (DataDeclType)
import Docs.Search.Types (ModuleName, PackageInfo, Identifier, PackageScore)

import Prelude
import Prim hiding (Type, Constraint)

import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Profunctor (wrapIso, dimap)
import Data.Tuple (Tuple)
import Data.Variant as Variant

-- | Metadata that makes sense only for certain types of search results.
data ResultInfo
  = DataResult
      { typeArguments :: Array TypeArgument
      , dataDeclType :: DataDeclType
      }
  | ExternDataResult { kind :: Type' }
  | TypeSynonymResult
      { arguments :: Array TypeArgument
      , type :: Type'
      }
  | DataConstructorResult
      { dataDeclType :: DataDeclType
      , type :: Type'
      }
  | TypeClassMemberResult
      { type :: Type'
      , typeClass :: Qualified (ProperName ClassName)
      , typeClassArguments :: Array TypeArgument
      }
  | TypeClassResult
      { fundeps :: FunDeps
      , arguments :: Array TypeArgument
      , superclasses :: Array Constraint'
      }
  | ValueResult { type :: Type' }
  | ValueAliasResult
  | TypeAliasResult
  | ExternKindResult

resultInfoCodec :: CA.JsonCodec ResultInfo
resultInfoCodec =
  dimap toVariant fromVariant $ CAV.variantMatch
    { data: Right $
        CAR.object "DataResult"
          { typeArguments: CA.array TypeDecoder.typeArgumentCodec
          , dataDeclType: TypeDecoder.dataDeclTypeCodec
          }
    , externData: Right TypeDecoder.typeCodec
    , typeSynonym: Right $
        CAR.object "TypeSynonymResult"
          { arguments: CA.array TypeDecoder.typeArgumentCodec
          , type: TypeDecoder.typeCodec
          }
    , dataConstructor: Right $
        CAR.object "DataConstructorResult"
          { dataDeclType: TypeDecoder.dataDeclTypeCodec
          , type: TypeDecoder.typeCodec
          }
    , typeClassMember: Right $
        CAR.object "TypeClassMemberResult"
          { type: TypeDecoder.typeCodec
          , typeClass: TypeDecoder.qualifiedNameCodec
          , typeClassArguments: CA.array TypeDecoder.typeArgumentCodec
          }
    , typeClass: Right $
        CAR.object "TypeClassResult"
          { fundeps: TypeDecoder.funDepsCodec
          , arguments: CA.array TypeDecoder.typeArgumentCodec
          , superclasses: CA.array TypeDecoder.constraintCodec
          }
    , value: Right TypeDecoder.typeCodec
    , valueAlias: Left unit
    , typeAlias: Left unit
    , externKind: Left unit
    }
  where
  toVariant = case _ of
    DataResult args -> inject @"data" args
    ExternDataResult args -> inject @"externData" args.kind
    TypeSynonymResult args -> inject @"typeSynonym" args
    DataConstructorResult args -> inject @"dataConstructor" args
    TypeClassMemberResult args -> inject @"typeClassMember" args
    TypeClassResult args -> inject @"typeClass" args
    ValueResult args -> inject @"value" args.type
    ValueAliasResult -> inject @"valueAlias" unit
    TypeAliasResult -> inject @"typeAlias" unit
    ExternKindResult -> inject @"externKind" unit

  fromVariant = Variant.match
    { data: DataResult
    , externData: \arg -> ExternDataResult { kind: arg }
    , typeSynonym: TypeSynonymResult
    , dataConstructor: DataConstructorResult
    , typeClassMember: TypeClassMemberResult
    , typeClass: TypeClassResult
    , value: \arg -> ValueResult { type: arg }
    , valueAlias: fromUnit ValueAliasResult
    , typeAlias: fromUnit TypeAliasResult
    , externKind: fromUnit ExternKindResult
    }

  fromUnit :: forall a. a -> Unit -> a
  fromUnit = const

-- | Extract the type field.
typeOf :: ResultInfo -> Maybe Type'
typeOf (TypeSynonymResult { type: res }) =
  Just res
typeOf (TypeClassMemberResult { type: res }) =
  Just res
typeOf (ValueResult { type: res }) =
  Just res
typeOf _ = Nothing

-- | Common metadata for all types of search results.
newtype SearchResult = SearchResult
  { name :: Identifier
  , comments :: Maybe String
  , hashAnchor :: String
  , moduleName :: ModuleName
  , packageInfo :: PackageInfo
  , score :: PackageScore
  , sourceSpan :: Maybe SourceSpan
  , info :: ResultInfo
  }

derive instance Newtype SearchResult _

searchResultCodec :: JsonCodec SearchResult
searchResultCodec = wrapIso SearchResult $
  CAR.object "SearchResult"
    { name: wrapIso Identifier $ CA.string
    , comments: CAR.optional CA.string
    , hashAnchor: CA.string
    , moduleName: Package.moduleNameCodec
    , packageInfo: Package.packageInfoCodec
    , score: Package.packageScoreCodec
    , sourceSpan: CAR.optional sourceSpanCodec
    , info: resultInfoCodec
    }

typeOfResult :: SearchResult -> Maybe Type'
typeOfResult = un SearchResult >>> (_.info) >>> typeOf
