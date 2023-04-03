module Spago.Paths where

import Spago.Prelude

import Effect.Unsafe (unsafePerformEffect)
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

localCacheStashesPath :: FilePath
localCacheStashesPath = Path.concat [ localCachePath, "stashes" ]

localCacheStashesDefaultsPath :: FilePath
localCacheStashesDefaultsPath = Path.concat [ localCacheStashesPath, "defaults" ]

localCacheStashesCustomPath :: FilePath
localCacheStashesCustomPath = Path.concat [ localCacheStashesPath, "custom" ]

localCachesStashEntireWorkspace :: FilePath
localCachesStashEntireWorkspace = mkLocalCachesDefaultStashFile "entire-workspace"

mkLocalCachesDefaultStashFile :: String -> FilePath
mkLocalCachesDefaultStashFile fileName = Path.concat [ localCacheStashesDefaultsPath, fileName <> ".stash" ]

mkLocalCachesCustomStashFile :: String -> FilePath
mkLocalCachesCustomStashFile fileName = Path.concat [ localCacheStashesCustomPath, fileName <> ".stash" ]
