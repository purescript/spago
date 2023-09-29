module Spago.Purs.Graph
  ( ImportCheckResult
  , ImportedPackages
  , checkImports
  , toImportErrors
  , runGraph
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Registry.Foreign.FastGlob as Glob
import Registry.PackageName as PackageName
import Spago.Config (Package(..), WithTestGlobs(..), WorkspacePackage, PackageMap)
import Spago.Config as Config
import Spago.Log as Log
import Spago.Paths as Paths
import Spago.Purs (ModuleGraph(..), ModuleGraphNode, ModuleName, Purs)
import Spago.Purs as Purs
import Unsafe.Coerce (unsafeCoerce)

type ImportCheckResult =
  { unused :: Set PackageName
  , unusedTest :: Set PackageName
  , transitive :: ImportedPackages
  , transitiveTest :: ImportedPackages
  }

type PreGraphEnv a =
  { packageDependencies :: Map PackageName PackageMap
  , logOptions :: LogOptions
  , purs :: Purs
  | a
  }

type GraphEnv a =
  { selected :: WorkspacePackage
  , graph :: Purs.ModuleGraph
  , packageDependencies :: Map PackageName PackageMap
  , logOptions :: LogOptions
  | a
  }

type PackageGraph = Map ModuleName PackageGraphNode

type PackageGraphNode =
  { path :: String
  , depends :: Array ModuleName
  , package :: PackageName
  }

-- | For every Package that we depend on, we note which Module we are depending on, 
-- | and for each of them, we note from which Module we are importing it.
-- | 
-- | Given code like
-- | ```
-- | module MyModule
-- | 
-- | import Prelude -- from the 'prelude' package
-- | ```
-- | This value would be
-- | `Map.singleton "prelude" $ Map.singleton "Prelude" $ Set.singleton "MyModule"``
type ImportedPackages = Map PackageName (Map ModuleName (Set ModuleName))

checkImports :: forall a. Spago (GraphEnv a) ImportCheckResult
checkImports = do
  { graph: ModuleGraph graph, selected, packageDependencies } <- ask

  let declaredDependencies = unwrap selected.package.dependencies
  let declaredTestDependencies = maybe Map.empty (unwrap <<< _.dependencies) selected.package.test
  let packageName = selected.package.name
  let testPackageName = unsafeCoerce (PackageName.print packageName <> ":test")

  -- First compile the globs for each package, we get out a set of the modules that a package contains
  -- and we can have a map from path to a PackageName
  let
    dependencies = foldl (Map.unionWith (\l _ -> l)) Map.empty packageDependencies
    allPackages = dependencies
      # Map.insert testPackageName (WorkspacePackage selected)
      # Map.insert packageName (WorkspacePackage selected)
  pathToPackage :: Map FilePath PackageName <- map (Map.fromFoldable <<< Array.fold)
    $ for (Map.toUnfoldable allPackages)
        \(Tuple name package) -> do
          -- Basically partition the modules of the current package by in src and test packages
          let withTestGlobs = if name == testPackageName then OnlyTestGlobs else NoTestGlobs
          globMatches :: Array FilePath <- map Array.fold $ traverse compileGlob (Config.sourceGlob withTestGlobs name package)
          pure $ map (\p -> Tuple p name) globMatches

  -- Compile the globs for the project, we get the set of source files in the project
  projectGlob :: Set FilePath <- map Set.fromFoldable do
    map Array.fold $ traverse compileGlob (Config.sourceGlob NoTestGlobs packageName (WorkspacePackage selected))

  -- Same but for tests
  projectTestsGlob :: Set FilePath <- map Set.fromFoldable do
    map Array.fold $ traverse compileGlob (Config.sourceGlob OnlyTestGlobs packageName (WorkspacePackage selected))

  let
    -- The direct dependencies specified in the config
    dependencyPackages = Map.mapMaybe (const (Just Map.empty)) declaredDependencies
    dependencyTestPackages = Map.mapMaybe (const (Just Map.empty))
      $ Map.union declaredDependencies declaredTestDependencies

    -- Using `pathToPackage`, add the PackageName to each module entry in the graph
    addPackageInfo :: PackageGraph -> Tuple ModuleName ModuleGraphNode -> PackageGraph
    addPackageInfo pkgGraph (Tuple moduleName { path, depends }) =
      let
        -- Windows paths will need a conversion to forward slashes to be matched to globs
        newPath = withForwardSlashes path
        newVal = do
          package <- Map.lookup newPath pathToPackage
          pure { path: newPath, depends, package }
      in
        maybe pkgGraph (\v -> Map.insert moduleName v pkgGraph) newVal
    packageGraph = foldl addPackageInfo Map.empty (Map.toUnfoldable graph :: Array _)

    -- Filter this improved graph to only have the project modules
    projectGraph = Map.filterWithKey (\_ { path } -> Set.member path projectGlob) packageGraph
    projectTestsGraph = Map.filterWithKey (\_ { path } -> Set.member path projectTestsGlob) packageGraph

    -- Go through all the modules in the project graph, figure out which packages each module depends on,
    -- accumulate all of that in a single place
    accumulateImported importedPkgs' (Tuple moduleName { depends }) =
      let
        accumulateDep importedPkgs importedModule = case Map.lookup importedModule packageGraph of
          Nothing -> importedPkgs
          -- Skip dependencies on modules in the same package, we are not interested in that
          Just { package } | package == packageName -> importedPkgs
          Just { package } | package == testPackageName -> importedPkgs
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
    importedTestPackages = foldl accumulateImported Map.empty (Map.toUnfoldable projectTestsGraph :: Array _)

    unused = Map.keys $ Map.difference dependencyPackages importedPackages
    transitive = Map.difference importedPackages dependencyPackages
    unusedTest =
      if Set.isEmpty projectTestsGlob then Set.empty
      else Map.keys $ Map.difference (Map.difference dependencyTestPackages dependencyPackages) importedTestPackages
    transitiveTest =
      if Set.isEmpty projectTestsGlob then Map.empty
      else Map.difference importedTestPackages dependencyTestPackages

  pure { unused, transitive, unusedTest, transitiveTest }

toImportErrors :: WorkspacePackage -> ImportCheckResult -> Array Docc
toImportErrors selected { unused, unusedTest, transitive, transitiveTest } =
  (if Set.isEmpty unused then [] else [ unusedError false selected unused ])
    <> (if Map.isEmpty transitive then [] else [ transitiveError false selected transitive ])
    <> (if Set.isEmpty unusedTest then [] else [ unusedError true selected unusedTest ])
    <> (if Map.isEmpty transitiveTest then [] else [ transitiveError true selected transitiveTest ])

compileGlob :: forall a. FilePath -> Spago (LogEnv a) (Array FilePath)
compileGlob sourcePath = do
  { succeeded, failed } <- Glob.match Paths.cwd [ withForwardSlashes sourcePath ]
  unless (Array.null failed) do
    logDebug [ toDoc "Encountered some globs that are not in cwd, proceeding anyways:", indent $ toDoc failed ]
  pure (succeeded <> failed)

runGraph :: forall a. Set FilePath -> Array String -> Spago (PreGraphEnv a) (Maybe Purs.ModuleGraph)
runGraph globs pursArgs = do
  maybeGraph <- Purs.graph globs pursArgs
  case maybeGraph of
    Left err -> do
      logWarn $ "Could not decode the output of `purs graph`, error: " <> CA.printJsonDecodeError err
      pure Nothing
    Right graph ->
      pure $ Just graph

unusedError :: Boolean -> WorkspacePackage -> Set PackageName -> Docc
unusedError isTest selected unused = toDoc
  [ toDoc $ (if isTest then "Tests for package '" else "Sources for package '")
      <> PackageName.print selected.package.name
      <> "' declares unused dependencies - please remove them from the project config:"
  , indent (toDoc (map (\p -> "- " <> PackageName.print p) (Set.toUnfoldable unused) :: Array _))
  ]

transitiveError :: Boolean -> WorkspacePackage -> ImportedPackages -> Docc
transitiveError isTest selected transitive = toDoc
  [ toDoc
      $ (if isTest then "Tests for package '" else "Sources for package '")
      <> PackageName.print selected.package.name
      <> "' import the following transitive dependencies - please add them to the project dependencies, or remove the imports:"
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
      $ "spago install "
      <> (if isTest then "--test-deps " else "")
      <> "-p "
      <> PackageName.print selected.package.name
      <> " "
      <> String.joinWith " " (map PackageName.print $ Set.toUnfoldable $ Map.keys transitive)
  ]
