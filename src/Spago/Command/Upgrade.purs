module Spago.Command.Upgrade where

import Spago.Prelude

import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Registry.Range as Range
import Registry.Version as Version
import Spago.Command.Fetch (FetchEnv)
import Spago.Command.Fetch as Fetch
import Spago.Config (WorkspacePackage)
import Spago.Config as Config
import Spago.Core.Config as Core
import Spago.FS as FS
import Spago.Path as Path
import Spago.Registry as Registry

type UpgradeArgs =
  { setVersion :: Maybe Version
  }

run :: ∀ a. UpgradeArgs -> Spago (FetchEnv a) Unit
run args = do
  { workspace, rootPath } <- ask
  doc <- justOrDieWith workspace.doc Config.configDocMissingErrorMessage

  case workspace.workspaceConfig.packageSet of
    Just (Core.SetFromRegistry { registry: currentPackageSet }) -> do
      latestPackageSet <- Registry.findPackageSet args.setVersion

      let whichVersion = args.setVersion # maybe "latest" (const "specified")

      case latestPackageSet <= currentPackageSet of
        true -> logSuccess $ "Nothing to upgrade, you already have the " <> whichVersion <> " package set."
        false -> do
          logInfo $ "Upgrading the package set to the " <> whichVersion <> " version: " <> Version.print latestPackageSet
          Config.setPackageSetVersionInConfig rootPath doc latestPackageSet
          logSuccess "Upgrade successful!"
    Just _ -> die "This command is not yet implemented for projects using a custom package set."
    Nothing -> do
      -- Solver-based project: upgrade dependency ranges to latest compatible versions
      let
        extraPackages = case workspace.packageSet.buildType of
          Config.RegistrySolverBuild ep -> ep
          _ -> Map.empty

      let workspacePackages = Config.getWorkspacePackages workspace.packageSet

      packagesToUpgrade <- case workspace.selected of
        Just wp -> pure (NEA.singleton wp)
        Nothing -> pure workspacePackages

      for_ packagesToUpgrade \workspacePackage -> do
        pkgDoc <- justOrDieWith workspacePackage.doc Config.configDocMissingErrorMessage
        upgradeSolverDeps workspacePackage pkgDoc extraPackages

upgradeSolverDeps :: forall a. WorkspacePackage -> YamlDoc Core.Config -> Config.PackageMap -> Spago (FetchEnv a) Unit
upgradeSolverDeps workspacePackage pkgDoc extraPackages = do
  -- Get current dependencies
  let currentDeps = Fetch.getWorkspacePackageDeps workspacePackage
  let currentCore = unwrap currentDeps.core
  let currentTest = unwrap currentDeps.test

  if Map.isEmpty currentCore && Map.isEmpty currentTest then
    logSuccess "No dependencies to upgrade."
  else do
    -- Widen all constraints to * and call solver for all dependencies combined
    let allWidened = map (const Core.widestRange) $ Map.union currentCore currentTest
    logInfo "Resolving latest compatible package versions..."
    allPlan <- Fetch.getTransitiveDepsFromRegistry allWidened extraPackages

    -- Split results based on original membership
    let corePlan = Map.filterKeys (\k -> Map.member k currentCore) allPlan
    let testPlan = Map.filterKeys (\k -> Map.member k currentTest) allPlan

    -- Upgrade constraints preserving their type:
    -- - Nothing (bare dep) stays Nothing
    -- - ExactVersion gets new exact version
    -- - VersionRange gets union with new caret (widest range stays widest)
    let
      upgradeConstraints :: Map PackageName (Maybe Core.VersionConstraint) -> Map PackageName Version -> Map PackageName (Maybe Core.VersionConstraint)
      upgradeConstraints oldDeps newVersions = Map.mapMaybeWithKey computeConstraint oldDeps
        where
        computeConstraint name maybeOldConstraint = do
          newVersion <- Map.lookup name newVersions
          pure case maybeOldConstraint of
            Nothing -> Nothing -- bare dep stays bare
            Just (Core.ExactVersion _) -> Just (Core.ExactVersion newVersion) -- exact stays exact
            Just (Core.VersionRange r)
              | r == Core.widestRange -> Just (Core.VersionRange Core.widestRange) -- "*" stays "*"
              | otherwise -> Just (Core.VersionRange (Range.union r (Range.caret newVersion)))

      upgradedCore = upgradeConstraints currentCore corePlan
      upgradedTest = upgradeConstraints currentTest testPlan

    -- Write back to config
    let configPath = workspacePackage.path </> "spago.yaml"
    logInfo $ "Updating dependency ranges in " <> Path.quote configPath

    unless (Map.isEmpty upgradedCore) do
      liftEffect $ Config.addConstraintsToConfig pkgDoc upgradedCore
    unless (Map.isEmpty upgradedTest) do
      liftEffect $ Config.addTestConstraintsToConfig pkgDoc upgradedTest

    liftAff $ FS.writeYamlDocFile configPath pkgDoc
    logSuccess "Upgrade successful!"
