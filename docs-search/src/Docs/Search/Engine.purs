module Docs.Search.Engine where

import Docs.Search.PackageIndex (PackageIndex, PackageResult)
import Docs.Search.SearchResult (SearchResult, typeOfResult)
import Docs.Search.TypeQuery (TypeQuery(..), parseTypeQuery, penalty)

import Prelude

import Data.Newtype (unwrap)
import Data.Array as Array
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Search.Trie (Trie)
import Data.String.Common (toLower) as String
import Data.List (List)


type Index = Trie Char (List SearchResult)


type Query m index input result =
  index -> input -> m { index :: index, results :: Array result }


type Engine m index typeIndex =
  { queryIndex
    :: Query m index String SearchResult
  , queryTypeIndex
    :: Query m typeIndex TypeQuery SearchResult
  , queryPackageIndex
    :: Query m PackageIndex String PackageResult
  }


type EngineState index typeIndex
  = { index :: index
    , typeIndex :: typeIndex
    , packageIndex :: PackageIndex
    }


mkEngineState
  :: forall index typeIndex
  .  index
  -> typeIndex
  -> PackageIndex
  -> EngineState index typeIndex
mkEngineState index typeIndex packageIndex =
  { index, typeIndex, packageIndex }


data Result
  = DeclResult SearchResult
  | TypeResult SearchResult
  | PackResult PackageResult


sortByPopularity :: Array Result -> Array Result
sortByPopularity =
  Array.sortWith
  \result ->
    -- Sort by popularity, show packages before
    -- ordinary definitions.
    - case result of
        DeclResult r -> 2 * (unwrap r).score
        TypeResult r -> 2 * (unwrap r).score
        PackResult r -> 2 * r.score + 1


query
  :: forall m index typeIndex
  .  Monad m
  => Engine m index typeIndex
  -> Query m (EngineState index typeIndex) String Result
query engine state input =
  case hush (parseTypeQuery input) >>= isValuableTypeQuery of

    -- A declaration/package query
    Nothing -> do
      let lower = String.toLower input

      response        <- engine.queryIndex        state.index        lower
      packageResponse <- engine.queryPackageIndex state.packageIndex lower

      pure { results: sortByPopularity $
             (packageResponse.results <#> PackResult) <>
             (response.results        <#> DeclResult)

             -- No need to update package index (it never changes).
           , index: state { index = response.index }
           }

    -- A type query
    Just typeQuery -> do

      response <- engine.queryTypeIndex state.typeIndex typeQuery

      pure { results: sortByDistance typeQuery (response.results) <#> TypeResult
           , index: state { typeIndex = response.index }
           }


isValuableTypeQuery :: TypeQuery -> Maybe TypeQuery
isValuableTypeQuery (QVar _) = Nothing
isValuableTypeQuery (QConst _) = Nothing
isValuableTypeQuery other = Just other


sortByDistance
  :: TypeQuery
  -> Array SearchResult
  -> Array SearchResult
sortByDistance typeQuery =
  Array.sortWith (map (penalty typeQuery) <<< typeOfResult)
