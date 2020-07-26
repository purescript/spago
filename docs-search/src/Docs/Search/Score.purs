module Docs.Search.Score where

import Docs.Search.Types (RawPackageName(..), PackageName(..), PackageInfo(..))

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.String.CodeUnits as String
import Web.Bower.PackageMeta (Dependencies, PackageMeta)


type Scores = Map PackageName Int

normalizePackageName :: RawPackageName -> PackageName
normalizePackageName (RawPackageName p) =
  fromMaybe (PackageName p) $ map wrap $ String.stripPrefix (wrap "purescript-") p


-- | Construct a mapping from package names to their scores, based on number
-- of reverse dependencies.
mkScores :: Array PackageMeta -> Scores
mkScores =
  Array.foldr
  (\pm ->
    updateScoresFor (unwrap pm).dependencies >>>
    updateScoresFor (unwrap pm).devDependencies
  )
  mempty

  where
    updateScoresFor :: Dependencies -> Scores -> Scores
    updateScoresFor deps scores =
      Array.foldr
      (\dep -> Map.insertWith add dep 1)
      scores
      (deps # unwrap >>> map (_.packageName >>> RawPackageName >>> normalizePackageName))


getPackageScore :: Scores -> PackageInfo -> Int
getPackageScore scores = case _ of
  Package p      -> getPackageScoreForPackageName scores p
  Builtin        -> 100000
  LocalPackage   -> 200000
  UnknownPackage -> 0


getPackageScoreForPackageName :: Scores -> PackageName -> Int
getPackageScoreForPackageName scores p = fromMaybe 0 $ Map.lookup p scores
