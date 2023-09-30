module Docs.Search.PackageIndex where

import Docs.Search.Config as Config
import Docs.Search.Extra (stringToList)
import Docs.Search.Score (Scores, getPackageScoreForPackageName)
import Docs.Search.Types (PackageName(..), PackageScore)
import Docs.Search.Loader as Loader

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Search.Trie (Trie)
import Data.Search.Trie as Trie
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Safe.Coerce (coerce)
import Web.Bower.PackageMeta as Bower

type PackageResult =
  { name :: PackageName
  , description :: Maybe String
  , score :: PackageScore
  , dependencies :: Array PackageName
  , repository :: Maybe String
  }

type PackageIndex = Trie Char PackageResult

type PackageInfo = Array PackageResult

mkPackageInfo :: Scores -> Array Bower.PackageMeta -> PackageInfo
mkPackageInfo packageScores pms =
  Array.fromFoldable
    $ Map.values
    $
      Array.foldr insert Map.empty pms

  where
  insert
    :: Bower.PackageMeta
    -> Map PackageName PackageResult
    -> Map PackageName PackageResult
  insert
    ( Bower.PackageMeta
        { name: Bower.PackageName name
        , description
        , dependencies
        , devDependencies
        , repository
        }
    ) =
    Map.insert
      packageName
      { name: packageName
      , description: description
      , score: getPackageScoreForPackageName packageScores packageName
      , dependencies: coerce $ dependencies <#> Tuple.fst
      {-
      dependencies <#>
        _.packageName >>> RawPackageName >>> normalizePackageName
        -}
      , repository: repository <#> unwrap >>> (_.url)
      }
    where
    packageName = PackageName name

mkScoresFromPackageIndex :: PackageIndex -> Scores
mkScoresFromPackageIndex =
  Trie.values >>> Array.fromFoldable >>>
    Array.foldr (\{ name, score } -> Map.insert name score) Map.empty

loadPackageIndex :: Aff PackageIndex
loadPackageIndex =
  mkPackageIndex <$> Loader.load Config.packageInfoItem Config.packageInfoLoadPath

mkPackageIndex :: PackageInfo -> PackageIndex
mkPackageIndex =
  Array.foldr
    (\package -> Trie.insert (stringToList $ unwrap package.name) package)
    mempty

queryPackageIndex
  :: forall m
   . Monad m
  => PackageIndex
  -> String
  -> m
       { index :: PackageIndex
       , results :: Array PackageResult
       }
queryPackageIndex index query =
  pure
    { index
    , results: Array.fromFoldable $ Trie.queryValues (stringToList query) index
    }
