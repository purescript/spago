module Spago.Command.Ls (listPackages, listPackageSet, LsEnv(..), LsDepsArgs, LsPackagesArgs) where

import Spago.Prelude

import Data.Array (replicate)
import Data.Foldable (elem, maximum)
import Data.Map (filterKeys)
import Data.Map as Map
import Data.Set (unions)
import Data.String.CodeUnits (fromCharArray, length)
import Data.Tuple.Nested (type (/\))
import Registry.Internal.Codec (packageMap)
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Config (Dependencies(..), Package(..), PackageSet, Workspace, getPackageLocation, packageCodec)
import Spago.Config as Config

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
    packagesToList = Map.toUnfoldable case transitive of
      true -> dependencies
      false ->
        let
          workspacePackages = Config.getWorkspacePackages workspace.packageSet
          toDependencyNames (Dependencies deps) = Map.keys deps
          directDependencies = unions $ ((toDependencyNames <<< _.dependencies <<< _.package) <$> workspacePackages)
        in
          filterKeys (_ `elem` directDependencies) dependencies
  case packagesToList of
    [] -> logWarn "There are no dependencies listed in your spago.dhall"
    _ -> output $ formatPackageNames json packagesToList

formatPackageNames :: Boolean -> Array (PackageName /\ Package) -> OutputFormat (Map PackageName Package)
formatPackageNames json = case json of
  true -> formatPackageNamesJson
  false -> formatPackageNamesText
  where
  formatPackageNamesJson :: Array (PackageName /\ Package) -> OutputFormat (Map PackageName Package)
  formatPackageNamesJson pkgs = OutputJson (packageMap packageCodec) (Map.fromFoldable pkgs)

  -- | Format all the package names from the configuration
  formatPackageNamesText :: Array (PackageName /\ Package) -> OutputFormat (Map PackageName Package)
  formatPackageNamesText pkgs =
    let
      -- TODO: Currently prints git/remote packages as local packages
      showLocation :: PackageName -> Package -> String
      showLocation _ (GitPackage gitPackage) = "Remote " <> surroundQuote gitPackage.git
      showLocation packageName package = "Local " <> surroundQuote (getPackageLocation packageName package)

      -- Calculated for indentation
      longestName = fromMaybe 0 $ maximum $ (length <<< PackageName.print <<< fst) <$> pkgs
      longestVersion = fromMaybe 0 $ maximum $ (length <<< showVersion <<< snd) <$> pkgs

      renderPkg :: PackageName /\ Package -> String
      renderPkg (packageName /\ package) =
        leftPad longestName (PackageName.print packageName)
          <> " "
          <> leftPad longestVersion (showVersion package)
          <> "   "
          <> showLocation packageName package
    in
      OutputLines $ renderPkg <$> pkgs

  showVersion :: Package -> String
  showVersion (RegistryVersion version) = "v" <> Version.print version
  showVersion (LocalPackage _) = "local"
  showVersion (GitPackage _) = "git"
  showVersion (WorkspacePackage _) = "workspace"

  leftPad :: Int -> String -> String
  leftPad n s
    | length s < n = s <> (fromCharArray $ replicate (n - length s) ' ')
    | otherwise = s

  surroundQuote :: String -> String
  surroundQuote y = "\"" <> y <> "\""
