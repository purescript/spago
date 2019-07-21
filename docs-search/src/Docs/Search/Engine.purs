module Docs.Search.Engine where

import Prelude

import Docs.Search.TypeQuery (TypeQuery(..), parseTypeQuery, penalty)
import Docs.Search.SearchResult (SearchResult, typeOf)
import Docs.Search.Index as Index
import Docs.Search.Index (Index)
import Docs.Search.TypeIndex as TypeIndex
import Docs.Search.TypeIndex (TypeIndex)

import Data.Array as Array
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.Common as String
import Effect.Aff (Aff)

data ResultsType = TypeResults TypeQuery | DeclResults

type State = { index :: Index
             , typeIndex :: TypeIndex
             }

query
  :: State
  -> String
  -> Aff { searchEngineState :: State
         , results :: Array SearchResult
         , resultsType :: ResultsType
         }
query { index, typeIndex } input =
  case hush (parseTypeQuery input) >>= isValuableTypeQuery of
    Nothing -> do
      response <- Index.query index (String.toLower input)
      pure { searchEngineState: { index: response.index, typeIndex }
           , results: response.results
           , resultsType: DeclResults }

    Just typeQuery -> do
      response <- TypeIndex.query typeIndex typeQuery
      pure { searchEngineState: { index, typeIndex: response.typeIndex }
           , results: sortByDistance typeQuery response.results
           , resultsType: TypeResults typeQuery }

isValuableTypeQuery :: TypeQuery -> Maybe TypeQuery
isValuableTypeQuery (QVar _) = Nothing
isValuableTypeQuery (QConst _) = Nothing
isValuableTypeQuery other = Just other

sortByDistance :: TypeQuery -> Array SearchResult -> Array SearchResult
sortByDistance typeQuery results =
  _.result <$> Array.sortBy comparePenalties resultsWithPenalties
  where
    comparePenalties r1 r2 = compare r1.penalty r2.penalty
    resultsWithPenalties =
      results <#>
      \result -> { penalty: typeOf (unwrap result).info <#> penalty typeQuery
                 , result }
