module Spago.Purs.Graph
  ( checkImports
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Registry.Foreign.FastGlob as Glob
import Spago.Config (Package(..), WorkspacePackage)
import Spago.Config as Config
import Spago.Paths as Paths
import Spago.Purs (ModuleGraph(..), ModuleGraphNode, ModuleName)
import Spago.Purs as Purs

type ImportCheckResult =
  { unused :: Set PackageName
  , transitive :: ImportedPackages
  }

type GraphEnv a =
  { selected :: WorkspacePackage
  , graph :: Purs.ModuleGraph
  , dependencies :: Map PackageName Package
  , logOptions :: LogOptions
  | a
  }

type PackageGraph = Map ModuleName PackageGraphNode

type PackageGraphNode =
  { path :: String
  , depends :: Array ModuleName
  , package :: PackageName
  }

-- Every package can be imported by several project modules.
-- In each project module, several modules from the same package can be imported.
type ImportedPackages = Map PackageName (Map ModuleName (Set ModuleName))

-- TODO: toggle the `hasTests` when running from the publishing flow
checkImports :: forall a. Spago (GraphEnv a) ImportCheckResult
checkImports = do
  { graph: ModuleGraph graph, selected, dependencies } <- ask

  -- First compile the globs for each package, we get out a set of the modules that a package contains
  pathToPackage :: Map FilePath PackageName <- map (Map.fromFoldable <<< Array.fold)
    $ for (Map.toUnfoldable $ Map.insert selected.package.name (WorkspacePackage selected) dependencies)
        \(Tuple name package) -> liftAff do
          globMatches :: Array FilePath <- map Array.fold $ traverse compileGlob (Config.sourceGlob name package)
          pure $ map (\p -> Tuple p name) globMatches

  projectGlob :: Set FilePath <- map Set.fromFoldable $ liftAff do
    map Array.fold $ traverse compileGlob (Config.sourceGlob selected.package.name (WorkspacePackage selected))

  let
    -- The direct dependencies specified in the config
    dependencyPackages = Map.mapMaybe (const (Just Map.empty)) $ unwrap selected.package.dependencies

    addPackageInfo :: PackageGraph -> Tuple ModuleName ModuleGraphNode -> PackageGraph
    addPackageInfo pkgGraph (Tuple moduleName { path, depends }) =
      let
        newVal = do
          package <- Map.lookup path pathToPackage
          pure { path, depends, package }
      in
        maybe pkgGraph (\v -> Map.insert moduleName v pkgGraph) newVal

    packageGraph = foldl addPackageInfo Map.empty (Map.toUnfoldable graph :: Array _)

    projectGraph = Map.filterWithKey (\_ { path } -> Set.member path projectGlob) packageGraph

    -- Go through all the modules in the project graph, figure out which packages each module depends on,
    -- accumulate all of that in a single place
    accumulateImported importedPkgs' (Tuple moduleName { depends }) =
      let
        accumulateDep importedPkgs importedModule = case Map.lookup importedModule packageGraph of
          Nothing -> importedPkgs
          -- Skip dependencies on modules in the same package, we are not interested in that
          Just { package } | package == selected.package.name -> importedPkgs
          Just { package: importedPackage } -> Map.alter
            ( case _ of
                Nothing -> Just $ Map.singleton moduleName (Set.singleton importedModule)
                Just p -> Just $ Map.alter
                  ( case _ of
                      Nothing -> Just $ Set.singleton importedModule
                      Just set -> Just $ Set.insert importedModule set
                  )
                  moduleName
                  p
            )
            importedPackage
            importedPkgs
      in
        foldl accumulateDep importedPkgs' depends

    importedPackages :: ImportedPackages
    importedPackages = foldl accumulateImported Map.empty (Map.toUnfoldable projectGraph :: Array _)

    unused = Map.keys $ Map.difference dependencyPackages importedPackages
    transitive = Map.difference importedPackages dependencyPackages

  pure { unused, transitive }

compileGlob :: FilePath -> Aff (Array FilePath)
compileGlob sourcePath = do
  { succeeded } <- Glob.match Paths.cwd [ sourcePath ]
  pure succeeded
