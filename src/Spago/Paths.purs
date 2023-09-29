module Spago.Paths where

import Prelude

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

mkRelative :: FilePath -> FilePath
mkRelative = Path.relative cwd

globalCachePath :: FilePath
globalCachePath = paths.cache

localCachePath :: FilePath
localCachePath = Path.concat [ cwd, ".spago" ]

localCachePackagesPath :: FilePath
localCachePackagesPath = Path.concat [ localCachePath, "packages" ]

registryPath ∷ FilePath
registryPath = Path.concat [ globalCachePath, "registry" ]

registryIndexPath ∷ FilePath
registryIndexPath = Path.concat [ globalCachePath, "registry-index" ]

packageSetsPath :: FilePath
packageSetsPath = Path.concat [ registryPath, "package-sets" ]

localCachePersistedWarningsPath :: FilePath
localCachePersistedWarningsPath = Path.concat [ localCachePath, "persisted-warnings" ]

localCachesPersistedWarningsEntireWorkspace :: FilePath
localCachesPersistedWarningsEntireWorkspace = mkLocalCachesPersistentWarningsFile "entire-workspace"

mkLocalCachesPersistentWarningsFile :: String -> FilePath
mkLocalCachesPersistentWarningsFile fileName = Path.concat [ localCachePersistedWarningsPath, fileName <> ".stash" ]
