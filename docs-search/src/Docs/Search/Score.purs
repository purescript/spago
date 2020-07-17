module Docs.Search.Score where

import Docs.Search.Types (PackageName)

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.String.CodeUnits as String
import Web.Bower.PackageMeta (Dependencies, PackageMeta)


type Scores = Map PackageName Int


getPackageScore :: Scores -> PackageName -> Int
getPackageScore _      "<builtin>" = 1000000
getPackageScore _      "<local package>" = 2000000
getPackageScore scores name = fromMaybe 0 $ Map.lookup name scores


normalizePackageName :: PackageName -> PackageName
normalizePackageName packageName =
  fromMaybe packageName $ String.stripPrefix (wrap "purescript-") packageName


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
      (deps # unwrap >>> map (_.packageName >>> normalizePackageName))
