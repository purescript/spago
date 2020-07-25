module Docs.Search.PackageIndex where

import Docs.Search.Config (config)
import Docs.Search.Extra (stringToList)
import Docs.Search.Score (Scores, getPackageScore, normalizePackageName)

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array as Array
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Search.Trie (Trie)
import Data.Search.Trie as Trie
import Effect (Effect)
import Effect.Aff (Aff)
import Web.Bower.PackageMeta (PackageMeta(..))


type PackageResult
  = { name :: String
    , description :: Maybe String
    , score :: Int
    , dependencies :: Array String
    , repository :: Maybe String
    }

type PackageIndex = Trie Char PackageResult

type PackageInfo = Array PackageResult


mkPackageInfo :: Scores -> Array PackageMeta -> PackageInfo
mkPackageInfo packageScores pms =
  Array.fromFoldable $
  Map.values $
  Array.foldr insert mempty pms

  where
    insert
      :: PackageMeta
      -> Map String PackageResult
      -> Map String PackageResult
    insert
      (PackageMeta { name
                   , description
                   , dependencies
                   , devDependencies
                   , repository }) =

          Map.insert
          name
          { name
          , description: description
          , score: getPackageScore packageScores $ normalizePackageName name
          , dependencies: unwrap dependencies <#> (_.packageName)
          , repository: repository <#> (_.url)
          }


mkScoresFromPackageIndex :: PackageIndex -> Scores
mkScoresFromPackageIndex =
  Trie.values >>> Array.foldr (\ { name, score } -> Map.insert name score) mempty


loadPackageIndex :: Aff PackageIndex
loadPackageIndex = do
  json <- toAffE (load config.packageInfoLoadPath)
  let packageInfo = fromMaybe mempty $ hush $ decodeJson json
  pure $ mkPackageIndex packageInfo


mkPackageIndex :: PackageInfo -> PackageIndex
mkPackageIndex =
  Array.foldr
  (\package -> Trie.insert (stringToList $ normalizePackageName package.name) package)
  mempty


queryPackageIndex
  :: forall m
  .  Monad m
  => PackageIndex
  -> String
  -> m { index :: PackageIndex
       , results :: Array PackageResult
       }
queryPackageIndex index query =
  pure { index
       , results: Array.fromFoldable $ Trie.queryValues (stringToList query) index
       }


foreign import load
  :: String
  -> Effect (Promise Json)
