module Spago.Command.Graph where

import Spago.Prelude

import Data.Array.NonEmpty as NEA
import Data.Codec.Argonaut.Common as CA
import Data.Graph as Data.Graph
import Data.List as List
import Record as Record
import Registry.PackageName as PackageName
import Spago.Command.Build as Build
import Spago.Command.Fetch as Fetch
import Spago.Config (Workspace)
import Spago.Config as Config
import Spago.Purs (Purs)
import Spago.Purs as Purs
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
graphModules b = do
  { dependencies, workspace } <- ask
  let allDependencies = Fetch.toAllDependencies dependencies
  let globs = Build.getBuildGlobs { selected: Config.getWorkspacePackages workspace.packageSet, withTests: false, dependencies: allDependencies, depsOnly: false }
  eitherGraph <- Graph.runGraph globs []
  graph <- either die pure eitherGraph
  output $ OutputJson Purs.moduleGraphCodec graph

graphPackages :: forall a. GraphPackagesArgs -> Spago (GraphEnv a) Unit
graphPackages { json, topo } = do
  env@{ dependencies, workspace } <- ask
  let allDependencies = Fetch.toAllDependencies dependencies
  let globs = Build.getBuildGlobs { selected: Config.getWorkspacePackages workspace.packageSet, withTests: false, dependencies: allDependencies, depsOnly: false }
  eitherGraph <- Graph.runGraph globs []
  graph <- either die pure eitherGraph
  let selected = unsafeFromJust $ NEA.fromArray $ Config.getWorkspacePackages workspace.packageSet
  packageGraph <- runSpago (Record.union { selected } env) (Graph.getPackageGraph graph)
  case topo of
    false -> output $ OutputJson Graph.packageGraphCodec packageGraph
    true -> output $ OutputJson (CA.list PackageName.codec)
      $ List.reverse
      $ Data.Graph.topologicalSort
      $ Data.Graph.fromMap
      $ map (\{ depends } -> Tuple unit $ List.fromFoldable depends) packageGraph

