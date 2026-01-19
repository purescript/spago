module Docs.Search.PackageIndex where

import Prelude

import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Search.Trie (Trie)
import Data.Search.Trie as Trie
import Docs.Search.Config as Config
import Docs.Search.Extra (stringToList)
import Docs.Search.Loader as Loader
import Docs.Search.Score (Scores, getPackageScoreForPackageName)
import Docs.Search.Types (PackageScore)
import Docs.Search.Types as Package
import Effect.Aff (Aff)
import Registry.Location (Location)
import Registry.Location as Location
import Registry.Manifest (Manifest(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName

type PackageResult =
  { name :: PackageName
  , description :: Maybe String
  , score :: PackageScore
  , dependencies :: Array PackageName
  , repository :: Location
  }

packageResultCodec :: CJ.Codec PackageResult
packageResultCodec = CJ.named "PackageResult" $
  CJ.Record.object
    { name: PackageName.codec
    , description: CJ.Record.optional CJ.string
    , score: Package.packageScoreCodec
    , dependencies: CJ.array PackageName.codec
    , repository: Location.codec
    }

type PackageIndex = Trie Char PackageResult

type PackageInfo = Array PackageResult

mkPackageInfo :: Scores -> Array Manifest -> PackageInfo
mkPackageInfo packageScores pms =
  Array.fromFoldable
    $ Map.values
    $ Array.foldr insert Map.empty pms

  where
  insert
    :: Manifest
    -> Map PackageName PackageResult
    -> Map PackageName PackageResult
  insert
    ( Manifest
        { name
        , description
        , dependencies
        , location
        }
    ) =
    Map.insert
      name
      { name
      , description
      , score: getPackageScoreForPackageName packageScores name
      , dependencies: Array.fromFoldable $ Map.keys dependencies
      , repository: location
      }

mkScoresFromPackageIndex :: PackageIndex -> Scores
mkScoresFromPackageIndex =
  Trie.values >>> Array.fromFoldable >>>
    Array.foldr (\{ name, score } -> Map.insert name score) Map.empty

loadPackageIndex :: Aff PackageIndex
loadPackageIndex =
  mkPackageIndex <$> Loader.load packageInfoCodec Config.packageInfoItem Config.packageInfoLoadPath
  where
  packageInfoCodec :: CJ.Codec PackageInfo
  packageInfoCodec = CJ.array packageResultCodec

mkPackageIndex :: PackageInfo -> PackageIndex
mkPackageIndex =
  Array.foldr
    (\package -> Trie.insert (stringToList $ PackageName.print package.name) package)
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
