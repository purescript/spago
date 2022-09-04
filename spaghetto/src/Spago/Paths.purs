module Spago.Paths where

import Spago.Prelude

import Effect.Unsafe (unsafePerformEffect)
import Node.Path as Path
import Node.Process as Process
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.PackageSet (Package(..))

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

globalCachePath :: FilePath
globalCachePath = paths.cache

localCachePath :: FilePath
localCachePath = Path.concat [ cwd, ".spaghetto" ] -- TODO: change to spago

localCachePackagesPath :: FilePath
localCachePackagesPath = Path.concat [ localCachePath, "packages" ]

getPackageLocation :: PackageName -> Package -> FilePath
getPackageLocation name = case _ of
  Version v -> Path.concat [ localCachePackagesPath, PackageName.print name <> "-" <> Version.printVersion v ]
  GitPackage p -> Path.concat [ localCachePackagesPath, PackageName.print name, p.ref ]

sourceGlob :: PackageName -> Package -> String
sourceGlob name package = Path.concat [ getPackageLocation name package, "src/**/*.purs" ]
