module Spago.Paths where

import Prelude

import Data.Array (cons, replicate, reverse)
import Data.String (joinWith)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process

type NodePaths =
  { config :: FilePath
  , data :: FilePath
  , cache :: FilePath
  , log :: FilePath
  , temp :: FilePath
  }

foreign import paths :: NodePaths

cwd :: FilePath
cwd = unsafePerformEffect (Process.cwd)

chdir :: forall m. MonadEffect m => FilePath -> m Unit
chdir dir = liftEffect $ Process.chdir dir

mkRelative :: FilePath -> FilePath
mkRelative = Path.relative cwd

globalCachePath :: FilePath
globalCachePath = paths.cache

localCachePath :: FilePath
localCachePath = toLocalCachePath cwd

localCachePackagesPath :: FilePath
localCachePackagesPath = toLocalCachePackagesPath cwd

localCacheGitPath :: FilePath
localCacheGitPath = toLocalCacheGitPath cwd

toLocalCachePath :: FilePath -> FilePath
toLocalCachePath rootDir = Path.concat [ rootDir, ".spago" ]

toLocalCachePackagesPath :: FilePath -> FilePath
toLocalCachePackagesPath rootDir = Path.concat [ toLocalCachePath rootDir, "p" ]

toLocalCacheGitPath :: FilePath -> FilePath
toLocalCacheGitPath rootDir = Path.concat [ toLocalCachePath rootDir, "g" ]

-- search maximum 4 levels up the tree to find all other `spago.yaml`, which may contain workspace definition
gitSearchDepth :: Int
gitSearchDepth = 4
toGitSearchPath :: FilePath -> Array FilePath
toGitSearchPath rootDir = reverse $ makeSearchPaths rootDir gitSearchDepth where
  makeSearchPath :: FilePath -> Int -> FilePath
  makeSearchPath wd i = joinWith "" $ cons wd $ cons "/" $ replicate i "../"

  makeSearchPaths :: FilePath -> Int -> Array FilePath
  makeSearchPaths _ 0 = mempty
  makeSearchPaths wd i | i > 0 = cons (makeSearchPath wd i) (makeSearchPaths wd (i - 1))
  makeSearchPaths _ _ = mempty

registryPath ∷ FilePath
registryPath = Path.concat [ globalCachePath, "registry" ]

registryIndexPath ∷ FilePath
registryIndexPath = Path.concat [ globalCachePath, "registry-index" ]

packageSetsPath :: FilePath
packageSetsPath = Path.concat [ registryPath, "package-sets" ]

-- | We should bump this number every time we change the database schema in a breaking way
databaseVersion :: Int
databaseVersion = 2

databasePath :: FilePath
databasePath = Path.concat [ globalCachePath, "spago.v" <> show databaseVersion <> ".sqlite" ]
