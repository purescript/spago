module Docs.Search.Score where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Docs.Search.Types (PackageInfo(..), PackageScore(..))
import Registry.Manifest (Manifest)
import Registry.PackageName (PackageName)
import Registry.Range (Range)
import Safe.Coerce (coerce)

type Scores = Map PackageName PackageScore

-- | Construct a mapping from package names to their scores, based on number
-- of reverse dependencies.
mkScores :: Array Manifest -> Scores
mkScores =
  Array.foldr (unwrap >>> _.dependencies >>> updateScoresFor) Map.empty

  where
  updateScoresFor :: Map PackageName Range -> Scores -> Scores
  updateScoresFor deps scores =
    Array.foldr
      (\(Tuple dep _) -> Map.insertWith add (coerce dep) one)
      scores
      (Map.toUnfoldable deps)

-- unsafeCrashWith "Docs.Search.Score"
getPackageScore :: Scores -> PackageInfo -> PackageScore
getPackageScore scores = case _ of
  Package p -> getPackageScoreForPackageName scores p
  Builtin -> PackageScore 100000
  LocalPackage p -> getPackageScoreForPackageName scores p * (PackageScore 2)
  UnknownPackage -> zero

getPackageScoreForPackageName :: Scores -> PackageName -> PackageScore
getPackageScoreForPackageName scores p = fromMaybe zero $ Map.lookup p scores
