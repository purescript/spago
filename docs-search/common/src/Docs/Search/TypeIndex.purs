-- | Partial type index, can be loaded on demand in the browser.
module Docs.Search.TypeIndex
  ( TypeIndex(..)
  , mkTypeIndex
  , query
  ) where

import Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Promise (Promise, toAffE)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Foldable (fold, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Set (Set)
import Docs.Search.Config as Config
import Docs.Search.Declarations (resultsForDeclaration)
import Docs.Search.DocTypes (Type')
import Docs.Search.Score (Scores)
import Docs.Search.SearchResult (ResultInfo(..), SearchResult(..))
import Docs.Search.SearchResult as SearchResult
import Docs.Search.TypeQuery (TypeQuery)
import Docs.Search.TypeShape (shapeOfType, shapeOfTypeQuery, stringifyShape)
import Effect (Effect)
import Effect.Aff (Aff, try)
import Effect.Aff as Error
import Effect.Class.Console as Console
import JSON (JSON)
import Language.PureScript.Docs.Types (DocModule(..))
import Registry.PackageName (PackageName)
import Spago.Purs.Types as Graph

newtype TypeIndex = TypeIndex (Map String (Maybe (Array SearchResult)))

derive instance newtypeTypeIndex :: Newtype TypeIndex _

mkTypeIndex :: Graph.ModuleGraphWithPackage -> Set PackageName -> Scores -> Array DocModule -> TypeIndex
mkTypeIndex moduleGraph workspacePackages scores docsJsons =
  TypeIndex $ map Just $ foldr insert Map.empty docsJsons
  where
  insert :: DocModule -> Map String (Array SearchResult) -> Map String (Array SearchResult)
  insert docsJson mp =
    Array.foldr
      ( \result ->
          case getType result of
            Just ty ->
              Map.insertWith append (stringifyShape $ shapeOfType ty) (pure result)
            Nothing -> identity
      )
      mp
      (allResults moduleGraph workspacePackages scores docsJson)

allResults :: Graph.ModuleGraphWithPackage -> Set PackageName -> Scores -> DocModule -> Array SearchResult
allResults moduleGraph workspacePackages scores (DocModule { name, declarations }) =
  declarations >>=
    ( resultsForDeclaration moduleGraph workspacePackages scores name
        >>> map (_.result)
        >>> Array.fromFoldable
    )

getType :: SearchResult -> Maybe Type'
getType (SearchResult { info }) =
  case info of
    ValueResult dict ->
      Just dict.type

    TypeClassMemberResult dict ->
      Just dict.type

    TypeSynonymResult dict ->
      Just dict.type

    _ -> Nothing

lookup
  :: String
  -> TypeIndex
  -> Aff { index :: TypeIndex, results :: Array SearchResult }
lookup key index@(TypeIndex map) =
  case Map.lookup key map of
    Just results ->
      pure { index, results: fold results }
    Nothing -> do
      eitherJson <- try $ toAffE $ lookup_ key (Config.mkShapeScriptPath key)

      let
        eitherResults = do
          json <- eitherJson # lmap Error.message
          CJ.decode (CJ.array SearchResult.searchResultCodec) json # lmap DecodeError.print

      case eitherResults of
        Right results ->
          pure { index: insert key (Just results) index, results }
        Left err -> do
          Console.error $ "Error reading type index: " <> err
          pure { index: insert key Nothing index, results: [] }

  where
  insert
    :: String
    -> Maybe (Array SearchResult)
    -> TypeIndex
    -> TypeIndex
  insert k v = over TypeIndex (Map.insert k v)

query
  :: TypeIndex
  -> TypeQuery
  -> Aff { index :: TypeIndex, results :: Array SearchResult }
query typeIndex typeQuery =
  lookup (stringifyShape $ shapeOfTypeQuery typeQuery) typeIndex

foreign import lookup_
  :: String
  -> String
  -> Effect (Promise JSON)
