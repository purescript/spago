module Spago.Command.Ls (listPackages, listPackageSet, LsEnv(..), LsDepsArgs, LsPackagesArgs) where

import Spago.Prelude

import Data.Foldable (elem)
import Data.Map (filterKeys)
import Data.Map as Map
import Data.Set (unions)
import Data.Tuple.Nested (type (/\))
import Registry.Internal.Codec (packageMap)
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Config (Dependencies(..), Package(..), PackageSet, Workspace)
import Spago.Config as Config
import Unsafe.Coerce (unsafeCoerce)

type LsPackagesArgs =
  { json :: Boolean
  }

type LsDepsArgs =
  { json :: Boolean
  , transitive :: Boolean
  }

type LsEnv =
  { dependencies :: PackageSet
  , logOptions :: LogOptions
  , workspace :: Workspace
  }

listPackageSet :: LsPackagesArgs -> Spago LsEnv Unit
listPackageSet { json } = do
  logDebug "Running `listPackageSet`"
  { workspace } <- ask
  output $ formatPackageNames json (Map.toUnfoldable workspace.packageSet)

listPackages :: LsDepsArgs -> Spago LsEnv Unit
listPackages { transitive, json } = do
  logDebug "Running `listPackages`"
  { dependencies, workspace } <- ask
  let
    directDependencies = filterKeys (_ `elem` direct) dependencies
      where
      toDependencyNames (Dependencies deps) = Map.keys deps
      packagesToNames = toDependencyNames <<< _.dependencies <<< _.package
      direct = unions (packagesToNames <$> workspacePackages)
      workspacePackages = case workspace.selected of
        Just p -> [ p ]
        Nothing -> Config.getWorkspacePackages workspace.packageSet

  let packages = Map.toUnfoldable $ if transitive then dependencies else directDependencies
  case packages of
    [] -> logWarn "There are no dependencies listed in your spago.dhall"
    _ -> output $ formatPackageNames json packages

formatPackageNames :: Boolean -> Array (PackageName /\ Package) -> OutputFormat (Map PackageName Package)
formatPackageNames json pkgs =
  if json then OutputJson (packageMap (unsafeCoerce 1)) (Map.fromFoldable pkgs) -- TODO
  else formatPackagesTable
  where
  formatPackagesTable = OutputTable
    { titles: [ "Package Type", "Version", "Location" ]
    , rows: toRow <$> pkgs
    }
    where
    toRow :: (PackageName /\ Package) -> Array String
    toRow (packageName /\ package) =
      [ PackageName.print packageName
      , showVersion package
      , showLocation package
      ]

    showLocation :: Package -> String
    showLocation (RegistryVersion _) = "-"
    showLocation (GitPackage { git }) = git
    showLocation (LocalPackage { path }) = path
    showLocation (WorkspacePackage { path }) = path

    showVersion :: Package -> String
    showVersion (RegistryVersion version) = Version.print version
    showVersion (GitPackage { ref }) = ref
    showVersion (LocalPackage _) = "local"
    showVersion (WorkspacePackage _) = "workspace"
