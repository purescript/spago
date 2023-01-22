module Spago.Purs.Graph
  ( checkImports
  , runGraphCheck
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Record as Record
import Registry.Foreign.FastGlob as Glob
import Registry.PackageName as PackageName
import Spago.Config (Package(..), WorkspacePackage)
import Spago.Config as Config
import Spago.Log as Log
import Spago.Paths as Paths
import Spago.Purs (ModuleGraph(..), ModuleGraphNode, ModuleName, Purs)
import Spago.Purs as Purs

type ImportCheckResult =
  { unused :: Set PackageName
  , transitive :: ImportedPackages
  }

type PreGraphEnv a =
  { dependencies :: Map PackageName Package
  , logOptions :: LogOptions
  , purs :: Purs
  | a
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
  -- and we can have a map from path to a PackageName
  pathToPackage :: Map FilePath PackageName <- map (Map.fromFoldable <<< Array.fold)
    $ for (Map.toUnfoldable $ Map.insert selected.package.name (WorkspacePackage selected) dependencies)
        \(Tuple name package) -> liftAff do
          globMatches :: Array FilePath <- map Array.fold $ traverse compileGlob (Config.sourceGlob name package)
          pure $ map (\p -> Tuple p name) globMatches

  -- Compile the globs for the project, we get the set of source files in the project
  projectGlob :: Set FilePath <- map Set.fromFoldable $ liftAff do
    map Array.fold $ traverse compileGlob (Config.sourceGlob selected.package.name (WorkspacePackage selected))

  let
    -- The direct dependencies specified in the config
    dependencyPackages = Map.mapMaybe (const (Just Map.empty)) $ unwrap selected.package.dependencies

    -- Using `pathToPackage`, add the PackageName to each module entry in the graph
    addPackageInfo :: PackageGraph -> Tuple ModuleName ModuleGraphNode -> PackageGraph
    addPackageInfo pkgGraph (Tuple moduleName { path, depends }) =
      let
        newVal = do
          package <- Map.lookup path pathToPackage
          pure { path, depends, package }
      in
        maybe pkgGraph (\v -> Map.insert moduleName v pkgGraph) newVal
    packageGraph = foldl addPackageInfo Map.empty (Map.toUnfoldable graph :: Array _)

    -- Filter this improved graph to only have the project modules
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

runGraphCheck :: forall a. WorkspacePackage -> Set FilePath -> Array String -> Spago (PreGraphEnv a) (Array (Array Docc))
runGraphCheck selected globs pursArgs = do
  env <- ask
  maybeGraph <- Purs.graph globs pursArgs
  case maybeGraph of
    Left err -> do
      logWarn $ "Could not decode the output of `purs graph`, error: " <> CA.printJsonDecodeError err
      pure []
    Right graph -> do
      { unused, transitive } <- runSpago (Record.union { graph, selected } env) checkImports

      let
        result =
          case Set.isEmpty unused of
            true -> []
            false -> [ unusedError selected unused ]
            <> case Map.isEmpty transitive of
              true -> []
              false -> [ transitiveError selected transitive ]

      pure result

unusedError :: WorkspacePackage -> Set PackageName -> Array Docc
unusedError selected unused =
  [ Log.break
  , toDoc $ "Package '" <> PackageName.print selected.package.name <> "' declares unused dependencies - please remove them from the project config:"
  , indent (toDoc (map (\p -> PackageName.print p) (Set.toUnfoldable unused) :: Array _))
  ]

transitiveError :: WorkspacePackage -> ImportedPackages -> Array Docc
transitiveError selected transitive =
  [ Log.break
  , toDoc $ "Package '" <> PackageName.print selected.package.name <> "' imports the following transitive dependencies - please add them to the project dependencies, or remove the imports:"
  , indent $ toDoc
      ( map
          ( \(Tuple p modules) -> toDoc
              [ toDoc $ PackageName.print p
              , indent $ toDoc
                  ( map
                      ( \(Tuple mod importedOnes) -> toDoc
                          [ toDoc $ "from `" <> mod <> "`, which imports:"
                          , indent $ toDoc (Array.fromFoldable importedOnes)
                          ]
                      )
                      (Map.toUnfoldable modules :: Array _)
                  )
              ]
          )
          (Map.toUnfoldable transitive)
          :: Array _
      )
  , Log.break
  , toDoc "Run the following command to install them all:"
  , indent $ toDoc
      $ "spago install -p "
      <> PackageName.print selected.package.name
      <> " "
      <> String.joinWith " " (map PackageName.print $ Set.toUnfoldable $ Map.keys transitive)
  ]
