module Spago.Command.Uninstall
  ( run
  , UninstallArgs
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.Newtype (wrap)
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Node.Path as Path
import Node.Process as Process
import Registry.PackageName as PackageName
import Spago.Command.Fetch (FetchEnv)
import Spago.Command.Fetch as Fetch
import Spago.Config (BuildType(..), Dependencies, Package(..), PackageConfig)
import Spago.Config as Config
import Spago.Config as Core
import Spago.FS as FS

type UninstallArgs =
  { dependenciesToRemove :: Set PackageName
  , testDeps :: Boolean
  }

run :: UninstallArgs -> Spago (FetchEnv _) Unit
run args = do
  logDebug "Running `spago uninstall`"
  { workspace } <- ask

  { sourceOrTestString, deps, configPath, yamlDoc, name } <- case workspace.selected, workspace.rootPackage of
    Just p, _ -> toContext (Path.concat [ p.path, "spago.yaml" ]) p.doc p.package
    Nothing, Just rootPackage -> toContext "spago.yaml" workspace.doc rootPackage
    Nothing, Nothing -> die "No package was selected. Please select a package."
  let
    { warn, removed: removedSet } = separate deps
    warnAbout = NEA.fromFoldable warn
    removed = NEA.fromFoldable removedSet
    newDeps = wrap $ Map.filterKeys (not <<< flip Set.member removedSet) $ unwrap deps
    modifyDoc = modifyConfig configPath yamlDoc sourceOrTestString

  logDebug $ "Existing " <> sourceOrTestString <> " dependencies are: " <> (String.joinWith ", " $ map PackageName.print $ Array.fromFoldable $ Map.keys $ unwrap deps)
  for_ warnAbout \undeclaredPkgs ->
    logWarn
      [ toDoc $ "The following packages cannot be uninstalled because they are not declared in the package's " <> sourceOrTestString <> " dependencies:"
      , indent2 $ toDoc $ String.joinWith ", " $ Array.fromFoldable $ map PackageName.print undeclaredPkgs
      ]

  case removed of
    Nothing -> do
      logWarn $ "The package config for " <> PackageName.print name <> " was not updated."
      -- We might be in a place where the config file is untouched, but we still need to update the lockfile
      case workspace.packageSet.lockfile of
        Right _ -> pure unit
        Left reason -> void $ writeNewLockfile reason
    Just removed' -> do
      modifyDoc removed'

      currentWorkspacePackage <- NEA.find (\p -> p.package.name == name) (Config.getWorkspacePackages workspace.packageSet) `justOrDieWith` "Impossible: package must be in workspace packages"
      let
        newWorkspacePackage = case args.testDeps of
          false -> currentWorkspacePackage { package { dependencies = newDeps } }
          true -> currentWorkspacePackage { package { test = currentWorkspacePackage.package.test # map (\t -> t { dependencies = newDeps }) } }

        newWorkspace = workspace
          { packageSet = workspace.packageSet
              { lockfile = Left "Lockfile is out of date (installing new packages)"
              -- If we are installing packages, we need to add the new deps to the selected package
              , buildType = case workspace.packageSet.buildType of
                  RegistrySolverBuild packageMap -> RegistrySolverBuild $ Map.insert newWorkspacePackage.package.name (WorkspacePackage newWorkspacePackage) packageMap
                  PackageSetBuild info packageMap -> PackageSetBuild info $ Map.insert newWorkspacePackage.package.name (WorkspacePackage newWorkspacePackage) packageMap
              }
          , selected = Just newWorkspacePackage
          }

      local (_ { workspace = newWorkspace }) do
        void $ writeNewLockfile "Lockfile is out of date (uninstalled packages)"

  where
  writeNewLockfile reason = do
    { workspace } <- ask
    dependencies <- traverse Fetch.getTransitiveDeps
      $ Map.fromFoldable
      $ map (\p -> Tuple p.package.name p)
      $ Config.getWorkspacePackages workspace.packageSet
    Fetch.writeNewLockfile reason dependencies

  toContext :: FilePath -> YamlDoc Core.Config -> PackageConfig -> Spago _ (_ _)
  toContext configPath yamlDoc pkgConfig = case args.testDeps of
    true -> case pkgConfig.test of
      Nothing -> do
        logWarn $ "Could not uninstall test dependencies for " <> PackageName.print pkgConfig.name <> " because it does not have a test configuration."
        liftEffect $ Process.exit' 0
      Just { dependencies } -> pure
        { name: pkgConfig.name
        , deps: dependencies
        , sourceOrTestString: "test"
        , yamlDoc
        , configPath
        }
    false -> pure
      { name: pkgConfig.name
      , deps: pkgConfig.dependencies
      , sourceOrTestString: "source"
      , yamlDoc
      , configPath
      }

  separate :: Dependencies -> { warn :: Set PackageName, removed :: Set PackageName }
  separate deps = foldl f { warn: mempty, removed: mempty } args.dependenciesToRemove
    where
    f :: _ -> PackageName -> _
    f acc next = case Map.member next (unwrap deps) of
      true -> acc { removed = Set.insert next acc.removed }
      false -> acc { warn = Set.insert next acc.warn }

  modifyConfig :: FilePath -> YamlDoc Core.Config -> String -> NonEmptyArray PackageName -> Spago (FetchEnv _) Unit
  modifyConfig configPath yamlDoc sourceOrTestString = \removedPackages -> do
    logInfo $ "Removing the following " <> sourceOrTestString <> " dependencies: "
      <> (String.joinWith ", " $ map PackageName.print $ Array.fromFoldable removedPackages)
    logDebug $ "Editing config file at path: " <> configPath
    liftEffect $ Config.removePackagesFromConfig yamlDoc args.testDeps $ NonEmptySet.fromFoldable1 removedPackages
    liftAff $ FS.writeYamlDocFile configPath yamlDoc
