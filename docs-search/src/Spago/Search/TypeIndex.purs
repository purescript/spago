module Spago.Search.TypeIndex where

import Data.Map
import Data.Maybe
import Data.Newtype
import Data.Tuple
import Effect.Aff
import Prelude
import Spago.Search.Index
import Spago.Search.TypeQuery
import Spago.Search.TypeShape
import Spago.Search.Config
import Spago.Search.SearchResult
import Spago.Search.Declarations

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json, fromString, stringify, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Search.Trie as Trie
import Effect (Effect)

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

insertTypes
  :: Tuple String SearchResult
  -> Map String (List SearchResult)
  -> Map String (List SearchResult)
insertTypes (Tuple path results) mp =
  Map.alter updateResults path mp
  where
    updateResults mbOldResults =
      case mbOldResults of
        Just oldResults ->
          Just $ results : oldResults
        Nothing ->
          Just $ List.singleton results

mkTypeIndex :: Declarations -> TypeIndex
mkTypeIndex (Declarations trie) = TypeIndex $ map (Array.fromFoldable >>> Just) types
  where
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
      let mbResult = do
            json <- hush eiJson
            results <- hush (decodeJson json)
            pure { index: insert key (Just results) index, results }
      case mbResult of
        Nothing -> pure { index: insert key Nothing index, results: [] }
        Just results -> pure results

-- TODO: flip arguments
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
