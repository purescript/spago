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

localCachesStashEntireWorkspace :: FilePath
localCachesStashEntireWorkspace = mkLocalCachesStashFile "entire-workspace"

mkLocalCachesStashFile :: String -> FilePath
mkLocalCachesStashFile fileName = Path.concat [ localCacheStashesPath, fileName <> ".stash" ]
