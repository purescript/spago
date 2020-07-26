module Docs.Search.PackageIndex where

import Docs.Search.Config (config)
import Docs.Search.Extra (stringToList)
import Docs.Search.Score (Scores, getPackageScoreForPackageName, normalizePackageName)
import Docs.Search.Types (PackageName, RawPackageName(..))

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
  = { name :: PackageName
    , description :: Maybe String
    , score :: Int
    , dependencies :: Array PackageName
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
      -> Map PackageName PackageResult
      -> Map PackageName PackageResult
    insert
      (PackageMeta { name
                   , description
                   , dependencies
                   , devDependencies
                   , repository }) =
        Map.insert
          packageName
          { name: packageName
          , description: description
          , score: getPackageScoreForPackageName packageScores packageName
          , dependencies:
            unwrap dependencies <#>
            (_.packageName >>> RawPackageName >>> normalizePackageName)
          , repository: repository <#> (_.url)
          }

      where packageName = normalizePackageName $ RawPackageName name

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
  (\package -> Trie.insert (stringToList $ unwrap package.name) package)
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
