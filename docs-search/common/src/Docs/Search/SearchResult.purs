module Docs.Search.SearchResult where

import Prelude

import Data.Codec.JSON.Variant as CJ.Variant
import Data.Codec.JSON.Common as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Profunctor (wrapIso, dimap)
import Data.Variant as Variant
import Docs.Search.DocTypes (DataDeclType, SourceSpan)
import Docs.Search.DocsJson as Docs
import Docs.Search.JsonCodec (inject)
import Docs.Search.TypeDecoder (ClassName, Constraint', FunDeps, ProperName, Qualified, Type', TypeArgument)
import Docs.Search.TypeDecoder as TypeDecoder
import Docs.Search.Types (Identifier(..), ModuleName, PackageInfo, PackageScore)
import Docs.Search.Types as Package

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

resultInfoCodec :: CJ.Codec ResultInfo
resultInfoCodec =
  dimap toVariant fromVariant $ CJ.Variant.variantMatch
    { data: Right $ CJ.named "DataResult" $
        CJ.Record.object
          { typeArguments: CJ.array TypeDecoder.typeArgumentCodec
          , dataDeclType: TypeDecoder.dataDeclTypeCodec
          }
    , externData: Right TypeDecoder.typeCodec
    , typeSynonym: Right $ CJ.named "TypeSynonymResult" $
        CJ.Record.object
          { arguments: CJ.array TypeDecoder.typeArgumentCodec
          , type: TypeDecoder.typeCodec
          }
    , dataConstructor: Right $ CJ.named "DataConstructorResult" $
        CJ.Record.object
          { dataDeclType: TypeDecoder.dataDeclTypeCodec
          , type: TypeDecoder.typeCodec
          }
    , typeClassMember: Right $ CJ.named "TypeClassMemberResult" $
        CJ.Record.object
          { type: TypeDecoder.typeCodec
          , typeClass: TypeDecoder.qualifiedNameCodec
          , typeClassArguments: CJ.array TypeDecoder.typeArgumentCodec
          }
    , typeClass: Right $ CJ.named "TypeClassResult" $
        CJ.Record.object
          { fundeps: TypeDecoder.funDepsCodec
          , arguments: CJ.array TypeDecoder.typeArgumentCodec
          , superclasses: CJ.array TypeDecoder.constraintCodec
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

searchResultCodec :: CJ.Codec SearchResult
searchResultCodec = wrapIso SearchResult $ CJ.named "SearchResult" $
  CJ.Record.object
    { name: wrapIso Identifier $ CJ.string
    , comments: CJ.Record.optional CJ.string
    , hashAnchor: CJ.string
    , moduleName: Package.moduleNameCodec
    , packageInfo: Package.packageInfoCodec
    , score: Package.packageScoreCodec
    , sourceSpan: CJ.Record.optional Docs.sourceSpanCodec
    , info: resultInfoCodec
    }

typeOfResult :: SearchResult -> Maybe Type'
typeOfResult = un SearchResult >>> (_.info) >>> typeOf
