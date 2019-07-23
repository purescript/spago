module Docs.Search.TypeIndex where

import Prelude

import Docs.Search.Config (config)
import Docs.Search.Declarations (resultsForEntry)
import Docs.Search.DocsJson (DocsJson(..))
import Docs.Search.SearchResult (ResultInfo(..), SearchResult)
import Docs.Search.TypeDecoder (Type)
import Docs.Search.TypeQuery (TypeQuery)
import Docs.Search.TypeShape (shapeOfType, shapeOfTypeQuery, stringifyShape)

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array as Array
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', isJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff, try)

newtype TypeIndex = TypeIndex (Map String (Maybe (Array SearchResult)))

derive newtype instance semigroupTypeIndex :: Semigroup TypeIndex
derive newtype instance monoidTypeIndex :: Monoid TypeIndex
derive instance newtypeTypeIndex :: Newtype TypeIndex _

mkTypeIndex :: Array DocsJson -> TypeIndex
mkTypeIndex docsJsons =
  TypeIndex $ map Just $ Array.foldr insert mempty docsJsons
  where
    insert :: DocsJson -> Map String (Array SearchResult) -> Map String (Array SearchResult)
    insert docsJson mp =
      Array.foldr (\result ->
                    case getType result of
                      Just ty ->
                        Map.insertWith append (stringifyShape $ shapeOfType ty) (pure result)
                      Nothing -> identity
                  ) mp (allResults docsJson)

allResults :: DocsJson -> Array SearchResult
allResults (DocsJson { name, declarations }) =
  declarations >>= (resultsForEntry name >>> map (_.result) >>> Array.fromFoldable)

resultsWithTypes :: DocsJson -> Array SearchResult
resultsWithTypes docsJson = Array.filter (getType >>> isJust) $ allResults docsJson

getType :: SearchResult -> Maybe Type
getType sr =
  case (unwrap sr).info of
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
  -> Aff { typeIndex :: TypeIndex, results :: Array SearchResult }
lookup key typeIndex@(TypeIndex map) =
  case Map.lookup key map of
    Just results -> pure { typeIndex, results: Array.fold results }
    Nothing -> do
      eiJson <- try (toAffE (lookup_ key $ config.mkShapeScriptPath key))
      pure $ fromMaybe'
        (\_ ->  { typeIndex: insert key Nothing typeIndex, results: [] })
        do
          json <- hush eiJson
          results <- hush (decodeJson json)
          pure { typeIndex: insert key (Just results) typeIndex, results }

  where
    insert
      :: String
      -> Maybe (Array SearchResult)
      -> TypeIndex
      -> TypeIndex
    insert k v = unwrap >>> Map.insert k v >>> wrap

query
  :: TypeIndex
  -> TypeQuery
  -> Aff { typeIndex :: TypeIndex, results :: Array SearchResult }
query typeIndex typeQuery = do
  res <- lookup (stringifyShape $ shapeOfTypeQuery typeQuery) typeIndex
  pure $ res { results = sortByRelevance typeQuery res.results }

-- | TODO
sortByRelevance :: TypeQuery -> Array SearchResult -> Array SearchResult
sortByRelevance typeQuery = identity

foreign import lookup_
  :: String
  -> String
  -> Effect (Promise Json)
