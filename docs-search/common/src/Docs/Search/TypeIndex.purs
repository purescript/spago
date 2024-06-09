-- | Partial type index, can be loaded on demand in the browser.
module Docs.Search.TypeIndex where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Either (hush)
import Data.Foldable (fold, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', isJust)
import Data.Newtype (class Newtype, over)
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
import JSON (JSON)
import Language.PureScript.Docs.Types (DocModule(..))

newtype TypeIndex = TypeIndex (Map String (Maybe (Array SearchResult)))

derive instance newtypeTypeIndex :: Newtype TypeIndex _

mkTypeIndex :: Scores -> Array DocModule -> TypeIndex
mkTypeIndex scores docsJsons =
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
      (allResults scores docsJson)

allResults :: Scores -> DocModule -> Array SearchResult
allResults scores (DocModule { name, declarations }) =
  declarations >>=
    ( resultsForDeclaration scores name
        >>> map (_.result)
        >>> Array.fromFoldable
    )

resultsWithTypes :: Scores -> DocModule -> Array SearchResult
resultsWithTypes scores = Array.filter (getType >>> isJust) <<< allResults scores

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
    Just results -> pure { index, results: fold results }
    Nothing -> do
      eiJson <- try (toAffE (lookup_ key $ Config.mkShapeScriptPath key))
      pure $ fromMaybe'
        (\_ -> { index: insert key Nothing index, results: [] })
        do
          json <- hush eiJson
          results <- hush (CJ.decode (CJ.array SearchResult.searchResultCodec) json)
          pure { index: insert key (Just results) index, results }

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
query typeIndex typeQuery = do
  res <- lookup (stringifyShape $ shapeOfTypeQuery typeQuery) typeIndex
  pure $ res { results = res.results }

foreign import lookup_
  :: String
  -> String
  -> Effect (Promise JSON)
