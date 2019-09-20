module Docs.Search.SearchResult where

import Docs.Search.DocsJson (DataDeclType)
import Docs.Search.TypeDecoder (Constraint, FunDeps, Kind, QualifiedName, Type, TypeArgument)

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)


-- | Metadata that makes sense only for certain types of search results.
data ResultInfo
  = DataResult            { typeArguments :: Array TypeArgument
                          , dataDeclType :: DataDeclType }
  | ExternDataResult      { kind :: Kind }
  | TypeSynonymResult     { arguments :: Array TypeArgument
                          , type :: Type }
  | DataConstructorResult { arguments :: Array Type }
  | TypeClassMemberResult { type :: Type
                          , typeClass :: QualifiedName
                          , typeClassArguments :: Array TypeArgument }
  | TypeClassResult       { fundeps :: FunDeps
                          , arguments :: Array TypeArgument
                          , superclasses :: Array Constraint }
  | ValueResult           { type :: Type }
  | ValueAliasResult
  | TypeAliasResult
  | ExternKindResult

derive instance genericResultInfo :: Generic ResultInfo _

instance encodeJsonResultInfo :: EncodeJson ResultInfo where
  encodeJson = genericEncodeJson

instance decodeJsonResultInfo :: DecodeJson ResultInfo where
  decodeJson = genericDecodeJson


-- | Extract the type field.
typeOf :: ResultInfo -> Maybe Type
typeOf (TypeSynonymResult { type: res }) =
  Just res
typeOf (TypeClassMemberResult { type: res }) =
  Just res
typeOf (ValueResult { type: res }) =
  Just res
typeOf _ = Nothing


-- | Common metadata for all types of search results.
newtype SearchResult
  = SearchResult
    { name :: String
    , comments :: Maybe String
    , hashAnchor :: String
    , moduleName :: String
    , packageName :: String
    , score :: Int
    , sourceSpan :: Maybe { start :: Array Int
                          , end :: Array Int
                          , name :: String
                          }
    , info :: ResultInfo
    }

derive instance genericSearchResult :: Generic SearchResult _
derive instance newtypeSearchResult :: Newtype SearchResult _

instance encodeJsonSearchResult :: EncodeJson SearchResult where
  encodeJson = genericEncodeJson

instance decodeJsonSearchResult :: DecodeJson SearchResult where
  decodeJson = genericDecodeJson


typeOfResult :: SearchResult -> Maybe Type
typeOfResult = un SearchResult >>> (_.info) >>> typeOf
