module Docs.Search.Score where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Docs.Search.Types (PackageInfo(..), PackageName, PackageScore(..))
import Safe.Coerce (coerce)
import Web.Bower.PackageMeta as Bower

type Dependencies = Array (Tuple Bower.PackageName Bower.VersionRange)
type Scores = Map PackageName PackageScore

-- | Construct a mapping from package names to their scores, based on number
-- of reverse dependencies.
mkScores :: Array Bower.PackageMeta -> Scores
mkScores =
  Array.foldr
    ( \(Bower.PackageMeta pm) ->
        updateScoresFor pm.dependencies >>>
          updateScoresFor pm.devDependencies
    )
    Map.empty

  where
  updateScoresFor :: Dependencies -> Scores -> Scores
  updateScoresFor deps scores =
    Array.foldr
      (\(Tuple dep _) -> Map.insertWith add (coerce dep) one)
      scores
      deps

-- unsafeCrashWith "Docs.Search.Score"
getPackageScore :: Scores -> PackageInfo -> PackageScore
getPackageScore scores = case _ of
  Package p -> getPackageScoreForPackageName scores p
  Builtin -> PackageScore 100000
  LocalPackage -> PackageScore 200000
  UnknownPackage -> zero

getPackageScoreForPackageName :: Scores -> PackageName -> PackageScore
getPackageScoreForPackageName scores p = fromMaybe zero $ Map.lookup p scores
