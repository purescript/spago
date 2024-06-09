module Spago.Command.Graph where

import Spago.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Graph as Data.Graph
import Data.List as List
import Data.Map as Map
import Record as Record
import Registry.PackageName as PackageName
import Spago.Command.Build as Build
import Spago.Command.Fetch as Fetch
import Spago.Config (Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Purs (Purs)
import Spago.Purs.Graph (ModuleGraphWithPackage, PackageGraph, ModuleGraphWithPackageNode)
import Spago.Purs.Graph as Graph

type GraphEnv a =
  { dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , workspace :: Workspace
  , purs :: Purs
  | a
  }

type GraphModulesArgs =
  { dot :: Boolean
  , json :: Boolean
  , topo :: Boolean
  }

type GraphPackagesArgs =
  { dot :: Boolean
  , json :: Boolean
  , topo :: Boolean
  }

graphModules :: forall a. GraphModulesArgs -> Spago (GraphEnv a) Unit
graphModules { dot, json, topo } = do
  env@{ dependencies, workspace } <- ask
  let allDependencies = Fetch.toAllDependencies dependencies
  let selected = Config.getWorkspacePackages workspace.packageSet
  let globs = Build.getBuildGlobs { selected, withTests: false, dependencies: allDependencies, depsOnly: false }
  eitherGraph <- Graph.runGraph globs []
  graph <- either die pure eitherGraph

  moduleGraph <- runSpago (Record.union { selected } env) (Graph.getModuleGraphWithPackage graph)
  case topo of
    false -> output case dot, json of
      true, _ -> OutputLines $ modulesToDot selected moduleGraph
      _, true -> OutputJson Graph.moduleGraphCodec moduleGraph
      _, _ -> OutputYaml Graph.moduleGraphCodec moduleGraph
    true ->
      let
        list = List.reverse
          $ Data.Graph.topologicalSort
          $ Data.Graph.fromMap
          $ map (\{ depends } -> Tuple unit $ List.fromFoldable depends) moduleGraph
      in
        output case json of
          true -> OutputJson (CJ.Common.list CJ.string) list
          false -> OutputLines $ Array.fromFoldable list

graphPackages :: forall a. GraphPackagesArgs -> Spago (GraphEnv a) Unit
graphPackages { dot, json, topo } = do
  env@{ dependencies, workspace } <- ask
  let allDependencies = Fetch.toAllDependencies dependencies
  let selected = Config.getWorkspacePackages workspace.packageSet
  let globs = Build.getBuildGlobs { selected, withTests: false, dependencies: allDependencies, depsOnly: false }
  eitherGraph <- Graph.runGraph globs []
  graph <- either die pure eitherGraph

  packageGraph <- runSpago (Record.union { selected } env) (Graph.getPackageGraph graph)
  case topo of
    false -> output case dot, json of
      true, _ -> OutputLines $ packagesToDot selected packageGraph
      _, true -> OutputJson Graph.packageGraphCodec packageGraph
      _, _ -> OutputYaml Graph.packageGraphCodec packageGraph
    true ->
      let
        list = List.reverse
          $ Data.Graph.topologicalSort
          $ Data.Graph.fromMap
          $ map (\{ depends } -> Tuple unit $ List.fromFoldable depends) packageGraph
      in
        output case json of
          true -> OutputJson (CJ.Common.list PackageName.codec) list
          false -> OutputLines $ map PackageName.print $ Array.fromFoldable list

packagesToDot :: NonEmptyArray WorkspacePackage -> PackageGraph -> Array String
packagesToDot selected graph =
  [ "strict digraph deps {", "node[shape=rect]", "splines=ortho" ]
    <> workspacePackages
    <> Array.foldMap packageToDot (Map.toUnfoldable graph)
    <> [ "}" ]
  where
  -- Workspace packages get a dashed border
  -- Note: we are calling `show` in a few places here because dot wants strings in quotes
  workspacePackages = NEA.toArray selected # map \{ package: { name } } -> show (PackageName.print name) <> " [style=dashed];"

  -- Each package gets a line for each dependency
  packageToDot :: Tuple PackageName { depends :: Set PackageName } -> Array String
  packageToDot (Tuple name { depends }) = Array.fromFoldable depends
    # map \dep -> show (PackageName.print name) <> " -> " <> show (PackageName.print dep) <> ";"

modulesToDot :: NonEmptyArray WorkspacePackage -> ModuleGraphWithPackage -> Array String
modulesToDot selected graph =
  [ "strict digraph modules {", "node[shape=rect]", "splines=ortho" ]
    <> workspaceModules
    <> Array.foldMap moduleToDot (Map.toUnfoldable graph)
    <> [ "}" ]
  where
  -- Workspace modules get a dashed border
  isInWorskpace :: ModuleGraphWithPackageNode -> Boolean
  isInWorskpace node = NEA.any (\{ package: { name } } -> name == node.package) selected
  workspaceModules = Map.filter isInWorskpace graph
    # Map.toUnfoldable
    # map \(Tuple moduleName _) -> show moduleName <> " [style=dashed];"

  -- Each module gets a line for each dependency
  moduleToDot :: Tuple String ModuleGraphWithPackageNode -> Array String
  moduleToDot (Tuple name { depends }) = Array.fromFoldable depends
    # map \dep -> show name <> " -> " <> show dep <> ";"
