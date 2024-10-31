module Spago.Purs.Graph
  ( ImportCheckResult
  , ImportedPackages
  , checkImports
  , toImportErrors
  , formatImportErrors
  , runGraph
  , PackageGraph
  , packageGraphCodec
  , getPackageGraph
  , ModuleGraphWithPackage
  , ModuleGraphWithPackageNode
  , moduleGraphCodec
  , getModuleGraphWithPackage
  ) where

import Spago.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Dodo as Doc
import Record as Record
import Registry.Internal.Codec as Internal.Codec
import Registry.PackageName as PackageName
import Spago.Command.Fetch as Fetch
import Spago.Config (Package(..), WithTestGlobs(..), WorkspacePackage)
import Spago.Config as Config
import Spago.Glob as Glob
import Spago.Log as Log
import Spago.Path as Path
import Spago.Purs (ModuleGraph(..), ModuleGraphNode, ModuleName, Purs)
import Spago.Purs as Purs
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Basics

type PreGraphEnv a =
  { dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , purs :: Purs
  | a
  }

runGraph :: ∀ a. RootPath -> Set LocalPath -> Array String -> Spago (PreGraphEnv a) (Either String Purs.ModuleGraph)
runGraph root globs pursArgs = map (lmap toErrorMessage) $ Purs.graph root globs pursArgs
  where
  toErrorMessage = append "Could not decode the output of `purs graph`, error: " <<< CJ.DecodeError.print

--------------------------------------------------------------------------------
-- Graph enriched with the package names

type PackageGraphEnv a =
  { selected :: NonEmptyArray WorkspacePackage
  , dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , rootPath :: RootPath
  | a
  }

type ModuleGraphWithPackageNode =
  { path :: String
  , depends :: Array ModuleName
  , package :: PackageName
  }

moduleGraphWithPackageNodeCodec :: CJ.Codec ModuleGraphWithPackageNode
moduleGraphWithPackageNodeCodec = CJ.named "ModuleGraphNode" $ CJ.Record.object
  { path: CJ.string
  , depends: CJ.array CJ.string
  , package: PackageName.codec
  }

type ModuleGraphWithPackage = Map ModuleName ModuleGraphWithPackageNode

moduleGraphCodec :: CJ.Codec ModuleGraphWithPackage
moduleGraphCodec = Internal.Codec.strMap "ModuleGraphWithPackage" Right identity moduleGraphWithPackageNodeCodec

getModuleGraphWithPackage :: forall a. Purs.ModuleGraph -> Spago (PackageGraphEnv a) ModuleGraphWithPackage
getModuleGraphWithPackage (ModuleGraph graph) = do
  { selected, dependencies, rootPath } <- ask

  -- First compile the globs for each package, we get out a set of the modules that a package contains
  -- and we can have a map from path to a PackageName
  let
    mkPackageEntry p = Tuple p.package.name (WorkspacePackage p)
    mkTestPackageEntry p = Tuple (unsafeCoerce (PackageName.print p.package.name <> ":test")) (WorkspacePackage p)
    testPackages = Map.fromFoldable $ map mkTestPackageEntry selected
    allDependencies = Fetch.toAllDependencies dependencies
    allPackages = allDependencies
      # Map.union (Map.fromFoldable $ map mkPackageEntry selected)
      # Map.union testPackages

  -- TODO: here we are calculating the globs for each package, potentially multiple times.
  -- We should memoise them, so that when we get the graph for a monorepo we don't evaluate the globs again.
  -- Each call is a few milliseconds, but there are potentially hundreds of those, and it adds up.
  logDebug "Calling pathToPackage..."
  pathToPackage :: Map LocalPath PackageName <- map (Map.fromFoldable <<< Array.fold)
    $ for (Map.toUnfoldable allPackages)
        \(Tuple name package) -> do
          -- Basically partition the modules of the current package by in src and test packages
          let withTestGlobs = if (Set.member name (Map.keys testPackages)) then OnlyTestGlobs else NoTestGlobs
          logDebug $ "Getting globs for package " <> PackageName.print name
          globMatches :: Array LocalPath <- map Array.fold $ traverse compileGlob (Config.sourceGlob rootPath withTestGlobs name package)
          pure $ map (\p -> Tuple p name) globMatches

  logDebug "Got the pathToPackage map, calling packageGraph"
  let
    -- Using `pathToPackage`, add the PackageName to each module entry in the graph
    addPackageInfo :: ModuleGraphWithPackage -> Tuple ModuleName ModuleGraphNode -> ModuleGraphWithPackage
    addPackageInfo pkgGraph (Tuple moduleName { path, depends }) =
      let
        -- Windows paths will need a conversion to forward slashes to be matched to globs
        newPath = withForwardSlashes $ rootPath </> path
        newVal = do
          package <- Map.lookup newPath pathToPackage
          pure { path: Path.localPart newPath, depends, package }
      in
        maybe pkgGraph (\v -> Map.insert moduleName v pkgGraph) newVal
    packageGraph = foldl addPackageInfo Map.empty (Map.toUnfoldable graph :: Array _)

  pure packageGraph

compileGlob :: ∀ a. LocalPath -> Spago { rootPath :: RootPath | a } (Array LocalPath)
compileGlob sourcePath = do
  { rootPath } <- ask
  liftAff $ Glob.gitignoringGlob
    { root: rootPath
    , includePatterns: [ Path.localPart $ withForwardSlashes sourcePath ]
    , ignorePatterns: []
    }

--------------------------------------------------------------------------------
-- Package graph

type PackageGraph = Map PackageName { depends :: Set PackageName }

packageGraphCodec :: CJ.Codec PackageGraph
packageGraphCodec = Internal.Codec.packageMap (CJ.named "PackageGraphNode" $ CJ.Record.object { depends: CJ.Common.set PackageName.codec })

getPackageGraph :: forall a. Purs.ModuleGraph -> Spago (PackageGraphEnv a) PackageGraph
getPackageGraph graph = do
  moduleGraphWithPackage <- getModuleGraphWithPackage graph
  let
    moduleToPackage moduleName = _.package <$> Map.lookup moduleName moduleGraphWithPackage

    moduleTupleToPackage { package: currentPackage, depends } =
      Map.singleton currentPackage
        -- No self-loops
        { depends: Set.filter (\p -> p /= currentPackage)
            $ Set.fromFoldable
            $ Array.mapMaybe moduleToPackage depends
        }

    packageGraph =
      -- No psci-support in the graph
      Map.filterKeys (\k -> Right k /= PackageName.parse "psci-support")
        $ foldl (Map.unionWith (\v1 v2 -> { depends: Set.union v1.depends v2.depends })) Map.empty
        $ map moduleTupleToPackage
        $ Map.values moduleGraphWithPackage

  pure packageGraph

--------------------------------------------------------------------------------
-- Graph of imported packages/modules

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

type ImportsGraphEnv a =
  { selected :: WorkspacePackage
  , workspacePackages :: NonEmptyArray WorkspacePackage
  , dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , rootPath :: RootPath
  | a
  }

checkImports :: forall a. ModuleGraph -> Spago (ImportsGraphEnv a) ImportCheckResult
checkImports graph = do
  env@{ selected, workspacePackages } <- ask
  packageGraph <- runSpago (Record.union { selected: workspacePackages } env) $ getModuleGraphWithPackage graph
  logDebug $ "Got the package graph for package " <> PackageName.print selected.package.name

  let
    dropValues = Map.mapMaybe (const (Just Map.empty))
    srcDeps = unwrap selected.package.dependencies
  srcResult <- getUsedUnusedTransitiveFor
    { selected
    , packageName: selected.package.name
    , dependencyPackages: dropValues srcDeps
    , isSrc: true
    , packageGraph
    }
  let srcDepsUsed = Map.filterKeys (flip Map.member srcResult.used) srcDeps
  testResult <- getUsedUnusedTransitiveFor
    { selected
    , packageName: selected.package.name
    , dependencyPackages:
        dropValues
          -- add the used source dependencies, not the declared ones.
          $ Map.unionWith const srcDepsUsed
          -- get test deps
          $ maybe Map.empty (unwrap <<< _.dependencies) selected.package.test
    , isSrc: false
    , packageGraph
    }

  pure
    { unused: srcResult.unused
    , transitive: srcResult.transitive
    , unusedTest: Set.difference testResult.unused $ Map.keys srcResult.used
    , transitiveTest: differenceAll testResult.transitive [ srcResult.used, srcResult.transitive ]
    }
  where
  differenceAll sourceMap removalsArray = foldl Map.difference sourceMap removalsArray
  getUsedUnusedTransitiveFor { selected, packageName, dependencyPackages, isSrc, packageGraph } = do
    let
      testPackageName = unsafeCoerce (PackageName.print packageName <> ":test")

      testGlobOption
        | isSrc = NoTestGlobs
        | otherwise = OnlyTestGlobs

    -- Compile the globs for the project, we get the set of source files in the project
    { rootPath } <- ask
    projectFiles :: Set String <-
      Config.sourceGlob rootPath testGlobOption packageName (WorkspacePackage selected)
        # traverse compileGlob
        <#> Array.fold
        <#> map (Path.localPart <<< withForwardSlashes)
        <#> Set.fromFoldable

    let
      -- Filter this improved graph to only have the project modules
      projectGraph = packageGraph # Map.filter \{ path } -> Set.member path projectFiles

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

    pure
      { used: if isSrc then Map.intersection dependencyPackages importedPackages else Map.empty
      , unused: Map.keys $ Map.difference dependencyPackages importedPackages
      , transitive: Map.difference importedPackages dependencyPackages
      }

--------------------------------------------------------------------------------
-- Errors

type ImportCheckResult =
  { unused :: Set PackageName
  , unusedTest :: Set PackageName
  , transitive :: ImportedPackages
  , transitiveTest :: ImportedPackages
  }

toImportErrors
  :: WorkspacePackage
  -> { reportSrc :: Boolean, reportTest :: Boolean }
  -> ImportCheckResult
  -> Array { errorMessage :: Docc, correction :: Docc }
toImportErrors selected opts { unused, unusedTest, transitive, transitiveTest } = do
  Array.catMaybes
    [ if opts.reportSrc && (not Set.isEmpty unused) then Just $ unusedError false selected unused else Nothing
    , if opts.reportSrc && (not Map.isEmpty transitive) then Just $ transitiveError false selected transitive else Nothing
    , if opts.reportTest && (not Set.isEmpty unusedTest) then Just $ unusedError true selected unusedTest else Nothing
    , if opts.reportTest && (not Map.isEmpty transitiveTest) then Just $ transitiveError true selected transitiveTest else Nothing
    ]

formatImportErrors :: Array { errorMessage :: Docc, correction :: Docc } -> Docc
formatImportErrors checkResults = do
  let
    separate { errorMessage, correction } = { errors: [ errorMessage ], fixes: [ correction ] }
    { errors, fixes } = foldMap separate checkResults
    blankLine = Doc.break <> Doc.break
  Array.fold
    [ toDoc "Found unused and/or undeclared transitive dependencies:"
    , blankLine
    , Array.intercalate blankLine errors
    , blankLine
    , Doc.lines
        [ toDoc "These errors can be fixed by running the below command(s):"
        , Array.intercalate Log.break fixes
        ]
    ]

unusedError :: Boolean -> WorkspacePackage -> Set PackageName -> { errorMessage :: Docc, correction :: Docc }
unusedError isTest selected unused = do
  let
    unusedPkgs :: Array String
    unusedPkgs = map PackageName.print $ Set.toUnfoldable unused
  { errorMessage: toDoc
      [ toDoc $ (if isTest then "Tests for package '" else "Sources for package '")
          <> PackageName.print selected.package.name
          <> "' declares unused dependencies - please remove them from the project config:"
      , indent $ toDoc $ map (append "- ") unusedPkgs
      ]
  , correction: toDoc
      $ "spago uninstall -p "
      <> PackageName.print selected.package.name
      <> (if isTest then " --test-deps" else "")
      <> " "
      <> String.joinWith " " unusedPkgs
  }

transitiveError :: Boolean -> WorkspacePackage -> ImportedPackages -> { errorMessage :: Docc, correction :: Docc }
transitiveError isTest selected transitive =
  { errorMessage: toDoc
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
      ]
  , correction: toDoc
      $ "spago install -p "
      <> PackageName.print selected.package.name
      <> (if isTest then " --test-deps" else "")
      <> " "
      <> String.joinWith " " (map PackageName.print $ Set.toUnfoldable $ Map.keys transitive)
  }
