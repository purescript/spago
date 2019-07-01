module Spago.Search.TypeIndex where

import Prelude

import Spago.Search.Config (config)
import Spago.Search.Declarations (Declarations(..))
import Spago.Search.SearchResult (ResultInfo(..), SearchResult)
import Spago.Search.TypeQuery (TypeQuery)
import Spago.Search.TypeShape (shapeOfType, shapeOfTypeQuery, stringifyShape)

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array as Array
import Data.Either (hush)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Search.Trie as Trie
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (Aff, try)

newtype TypeIndex = TypeIndex (Map String (Maybe (Array SearchResult)))

derive newtype instance semigroupTypeIndex :: Semigroup TypeIndex
derive newtype instance monoidTypeIndex :: Monoid TypeIndex
derive instance newtypeTypeIndex :: Newtype TypeIndex _

insert
 :: String
 -> Maybe (Array SearchResult)
 -> TypeIndex
 -> TypeIndex
insert key value = unwrap >>> Map.insert key value >>> wrap

mkTypeIndex :: Declarations -> TypeIndex
mkTypeIndex (Declarations trie) = TypeIndex $ map (Array.fromFoldable >>> Just) types
  where
    insertTypes
      :: Tuple String SearchResult
      -> Map String (List SearchResult)
      -> Map String (List SearchResult)
    insertTypes (Tuple shape result) =
      Map.insertWith append shape (List.singleton result)

    types = List.foldr insertTypes mempty do

      results <- Trie.entriesUnordered trie >>= snd

      case (unwrap results).info of
        ValueResult dict ->
          insertTypeResultsFor dict.type results

        TypeClassMemberResult dict ->
          insertTypeResultsFor dict.type results

        TypeSynonymResult dict ->
          insertTypeResultsFor dict.type results
        _ -> mempty

    insertTypeResultsFor ty results =
      let path = stringifyShape (shapeOfType ty) in
      pure $ Tuple path results

lookup
  :: String
  -> TypeIndex
  -> Aff { index :: TypeIndex, results :: Array SearchResult }
lookup key index@(TypeIndex map) =
  case Map.lookup key map of
    Just results -> pure { index, results: Array.fold results }
    Nothing -> do
      eiJson <- try (toAffE (lookup_ key $ config.mkShapeScriptPath key))
      pure $ fromMaybe'
        (\_ ->  { index: insert key Nothing index, results: [] })
        do
          json <- hush eiJson
          results <- hush (decodeJson json)
          pure { index: insert key (Just results) index, results }

query
  :: TypeIndex
  -> TypeQuery
  -> Aff { index :: TypeIndex, results :: Array SearchResult }
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
