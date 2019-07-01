module Spago.Search.SearchResult where

import Spago.Search.DocsJson (ChildDeclType(..), ChildDeclaration(..), DataDeclType, DeclType(..), Declaration(..), DocsJson(..))
import Spago.Search.TypeDecoder (Constraint(..), FunDeps, Kind, QualifiedName(..), Type(..), TypeArgument)
import Spago.Search.TypeShape (ShapeChunk, joinForAlls, shapeOfType)

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Foldable (foldr)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Search.Trie (Trie, alter)
import Data.String.CodeUnits (stripPrefix, stripSuffix, toCharArray)
import Data.String.Common (toLower)
import Data.String.Common as String
import Data.String.Pattern (Pattern(..))

import Data.Generic.Rep (class Generic)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)

data ResultInfo
  = DataResult            { typeArguments :: Array TypeArgument
                          , dataDeclType :: DataDeclType }
  | ExternDataResult      { kind :: Kind }
  | TypeSynonymResult     { arguments :: Array TypeArgument
                          , type :: Type }
  | DataConstructorResult { arguments :: Array Type }
  | TypeClassMemberResult { type :: Type
                          , typeClass :: QualifiedName
                          , typeClassArguments :: Array String }
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

newtype SearchResult
  = SearchResult { name :: String
                 , comments :: Maybe String
                 , hashAnchor :: String
                 , moduleName :: String
                 , packageName :: String
                 , sourceSpan :: Maybe { start :: Array Int
                                       , end :: Array Int
                                       , name :: String
                                       }
                 , info :: ResultInfo
                 }

derive instance newtypeSearchResult :: Newtype SearchResult _
derive instance genericSearchResult :: Generic SearchResult _

instance encodeJsonSearchResult :: EncodeJson SearchResult where
  encodeJson = genericEncodeJson

instance decodeJsonSearchResult :: DecodeJson SearchResult where
  decodeJson = genericDecodeJson
