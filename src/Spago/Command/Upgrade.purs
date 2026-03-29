module Spago.Command.Upgrade where

import Spago.Prelude

import Data.Array.NonEmpty as NEA
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Version as Version
import Spago.Command.Build as Build
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

type UpgradePlan =
  { workspacePackage :: WorkspacePackage
  , pkgDoc :: YamlDoc Core.Config
  , currentCore :: Map PackageName (Maybe Core.VersionConstraint)
  , currentTest :: Map PackageName (Maybe Core.VersionConstraint)
  , upgradedCore :: Map PackageName (Maybe Core.VersionConstraint)
  , upgradedTest :: Map PackageName (Maybe Core.VersionConstraint)
  , resolvedVersions :: Map PackageName Version
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
      { logOptions, git, purs } <- ask
      let
        extraPackages = case workspace.packageSet.buildType of
          Config.RegistrySolverBuild ep -> ep
          _ -> Map.empty

      let workspacePackages = Config.getWorkspacePackages workspace.packageSet

      packagesToUpgrade <- case workspace.selected of
        Just wp -> pure (NEA.singleton wp)
        Nothing -> pure workspacePackages

      -- (1) compute all upgrade plans
      upgradePlans <- for packagesToUpgrade \workspacePackage -> do
        pkgDoc <- justOrDieWith workspacePackage.doc Config.configDocMissingErrorMessage
        computeUpgradePlan workspacePackage pkgDoc extraPackages

      -- (2) build all to verify upgraded dependencies work
      logInfo "Building with upgraded dependencies to verify compatibility..."

      -- Construct PackageTransitiveDeps from resolved versions
      let
        plans = NEA.toArray upgradePlans
        toPkgMap plan = plan.resolvedVersions # mapWithIndex \pkgName version ->
          fromMaybe (Config.RegistryVersion version) (Map.lookup pkgName extraPackages)

        dependencies :: Fetch.PackageTransitiveDeps
        dependencies = Map.fromFoldable $ plans <#>
          \p -> p.workspacePackage.package.name /\ { core: toPkgMap p, test: Map.empty }

      -- Install all, construct a BuildEnv and run the build
      Fetch.fetchPackagesToLocalCache (Fetch.toAllDependencies dependencies)
      let
        buildEnv =
          { logOptions
          , rootPath
          , purs
          , git
          , dependencies
          , workspace
          , strictWarnings: Nothing
          , pedanticPackages: false
          }
      buildSuccess <- runSpago buildEnv (Build.run { depsOnly: false, pursArgs: [], jsonErrors: false })

      unless buildSuccess do
        die
          [ "Build failed with upgraded dependencies. Config was not modified."
          , "Check the build errors above to identify incompatible packages."
          ]

      -- (3) persist config changes only if there are actual upgrades
      for_ plans \plan -> do
        let hasChanges = plan.upgradedCore /= plan.currentCore || plan.upgradedTest /= plan.currentTest
        when hasChanges do
          let configPath = plan.workspacePackage.path </> "spago.yaml"
          logInfo $ "Updating dependency ranges in " <> Path.quote configPath
          unless (Map.isEmpty plan.upgradedCore) do
            liftEffect $ Config.addConstraintsToConfig plan.pkgDoc plan.upgradedCore
          unless (Map.isEmpty plan.upgradedTest) do
            liftEffect $ Config.addTestConstraintsToConfig plan.pkgDoc plan.upgradedTest
          liftAff $ FS.writeYamlDocFile configPath plan.pkgDoc

      logSuccess "Upgrade successful!"

-- | Computes an upgrade plan for a package without persisting changes.
computeUpgradePlan :: forall a. WorkspacePackage -> YamlDoc Core.Config -> Config.PackageMap -> Spago (FetchEnv a) UpgradePlan
computeUpgradePlan workspacePackage pkgDoc extraPackages = do
  -- Get current dependencies
  let currentDeps = Fetch.getWorkspacePackageDeps workspacePackage
  let currentCore = unwrap currentDeps.core
  let currentTest = unwrap currentDeps.test

  -- Widen all constraints to * and call solver for all dependencies combined
  let allWidened = map (const Core.widestRange) $ Map.union currentCore currentTest
  logInfo $ "Resolving latest compatible versions for " <> PackageName.print workspacePackage.package.name <> "..."
  allPlan <- Fetch.getTransitiveDepsFromRegistry allWidened extraPackages

  -- Upgrade constraints preserving their type:
  -- - If solver didn't resolve the dep, keep the old constraint
  -- - Nothing (bare dep) stays Nothing
  -- - ExactVersion gets new exact version
  -- - VersionRange gets union with new caret (widest range stays widest)
  let
    upgradeConstraints :: Map PackageName (Maybe Core.VersionConstraint) -> Map PackageName Version -> Map PackageName (Maybe Core.VersionConstraint)
    upgradeConstraints oldDeps newVersions = mapWithIndex computeConstraint oldDeps
      where
      computeConstraint name maybeOldConstraint =
        case Map.lookup name newVersions of
          Nothing -> maybeOldConstraint -- solver didn't resolve, keep old
          Just newVersion -> case maybeOldConstraint of
            Nothing -> Nothing -- bare dep stays bare
            Just (Core.ExactVersion _) -> Just (Core.ExactVersion newVersion) -- exact stays exact
            Just (Core.VersionRange r)
              | r == Core.widestRange -> Just (Core.VersionRange Core.widestRange) -- "*" stays "*"
              | otherwise -> Just (Core.VersionRange (Range.union r (Range.caret newVersion)))

    upgradedCore = upgradeConstraints currentCore allPlan
    upgradedTest = upgradeConstraints currentTest allPlan

  pure { workspacePackage, pkgDoc, currentCore, currentTest, upgradedCore, upgradedTest, resolvedVersions: allPlan }
