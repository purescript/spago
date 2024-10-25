module Spago.Paths
  ( chdir
  , cwd
  , databasePath
  , databaseVersion
  , globalCachePath
  , localCacheGitPath
  , localCachePackagesPath
  , localCachePath
  , packageSetsPath
  , paths
  , registryIndexPath
  , registryPath
  ) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Node.Path as Node.Path
import Node.Process as Process
import Spago.Path (class IsPath, AdHocFilePath, GlobalPath, global, toRaw, (</>))

type NodePaths p =
  { config :: p
  , data :: p
  , cache :: p
  , log :: p
  , temp :: p
  }

foreign import paths_ :: NodePaths String

paths :: NodePaths GlobalPath
paths =
  { config: global paths_.config
  , data: global paths_.data
  , cache: global paths_.cache
  , log: global paths_.log
  , temp: global paths_.temp
  }

cwd :: ∀ m. MonadEffect m => m GlobalPath
cwd = global <$> liftEffect Process.cwd

chdir :: ∀ m path. MonadEffect m => IsPath path => path -> m Unit
chdir path = liftEffect $ Process.chdir (toRaw path)

globalCachePath :: GlobalPath
globalCachePath = paths.cache

localCachePath :: AdHocFilePath
localCachePath = ".spago"

localCachePackagesPath :: AdHocFilePath
localCachePackagesPath = Node.Path.concat [ localCachePath, "p" ]

localCacheGitPath :: AdHocFilePath
localCacheGitPath = Node.Path.concat [ localCachePath, "g" ]

registryPath ∷ GlobalPath
registryPath = globalCachePath </> "registry"

registryIndexPath ∷ GlobalPath
registryIndexPath = globalCachePath </> "registry-index"

packageSetsPath :: GlobalPath
packageSetsPath = registryPath </> "package-sets"

-- | We should bump this number every time we change the database schema in a breaking way
databaseVersion :: Int
databaseVersion = 2

databasePath :: GlobalPath
databasePath = globalCachePath </> ("spago.v" <> show databaseVersion <> ".sqlite")
