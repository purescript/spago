module Spago.Command.Build
  ( run
  , BuildEnv
  , SelectedPackageGlob(..)
  , getBuildGlobs
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Either (note)
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple as Tuple
import Dodo as Dodo
import Effect.Ref as Ref
import Record as Record
import Registry.PackageName as PackageName
import Spago.BuildInfo as BuildInfo
import Spago.Cmd as Cmd
import Spago.Command.Fetch as Fetch
import Spago.Config (Package(..), PackageMap, PackageSet, WithTestGlobs(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Git (Git)
import Spago.Psa as Psa
import Spago.Psa.Types as PsaTypes
import Spago.Purs (Purs)
import Spago.Purs as Purs
import Spago.Purs.Graph as Graph

type BuildEnv a =
  { purs :: Purs
  , git :: Git
  , dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , workspace :: Workspace
  , psaCliFlags :: PsaTypes.PsaOutputOptions
  | a
  }

type BuildOptions =
  { depsOnly :: Boolean
  , pursArgs :: Array String
  , jsonErrors :: Boolean
  }

run :: forall a. BuildOptions -> Spago (BuildEnv a) Unit
run opts = do
  logInfo "Building..."
  { dependencies
  , workspace
  , logOptions
  , psaCliFlags
  } <- ask

  BuildInfo.writeBuildInfo

  -- find the output flag and die if it's there - Spago handles it
  when (isJust $ Cmd.findFlag { flags: [ "-o", "--output" ], args: opts.pursArgs }) do
    die
      [ "Can't pass `--output` option directly to purs."
      , "Use the --output flag for Spago, or add it to your config file."
      ]
  let
    addOutputArgs args = case workspace.buildOptions.output of
      Nothing -> args
      Just output -> args <> [ "--output", output ]

  -- find the `--json-errors` flag and die if it's there - Spago handles it
  when (isJust $ Cmd.findFlag { flags: [ "--json-errors" ], args: opts.pursArgs }) do
    die
      [ "Can't pass `--json-errors` option directly to purs."
      , "Use the --json-errors flag for Spago."
      ]

  {-
  TODO: before, then, else
      buildAction globs = do
        let action = buildBackend globs >> (fromMaybe (pure ()) maybePostBuild)
        runCommands "Before" beforeCommands
        action `onException` (runCommands "Else" elseCommands)
        runCommands "Then" thenCommands
  -}

  when (isJust $ Cmd.findFlag { flags: [ "-g", "--codegen" ], args: opts.pursArgs }) do
    die
      [ "Can't pass the `--codegen` option to purs, Spago already does that for you."
      , "Remove the argument to make this error go away!"
      ]
  let
    args = (addOutputArgs opts.pursArgs) <>
      [ "--codegen"
      , "corefn,docs" <> case workspace.backend of
          -- If there's no backend specified then we do compile to JS, otherwise skip the target.
          -- Note: sourcemaps are JS specific, so they go together
          Nothing -> ",js,sourcemaps"
          Just _ -> ""
      ]

  selectedPackages :: Array (Tuple WorkspacePackage PackageMap) <- case workspace.selected of
    Just p -> case Map.lookup p.package.name dependencies of
      Just allDeps -> pure [ Tuple p allDeps ]
      Nothing -> die [ "Internal error. Did not get transitive dependencies for selected package: " <> PackageName.print p.package.name ]
    Nothing -> do
      let
        getTransDeps :: WorkspacePackage -> Either WorkspacePackage PackageMap
        getTransDeps p = note p $ Map.lookup p.package.name dependencies

        result :: Either WorkspacePackage (Array (Tuple WorkspacePackage PackageMap))
        result = traverse (\p -> Tuple p <$> getTransDeps p) $ Config.getTopologicallySortedWorkspacePackages workspace.packageSet
      case result of
        Right a -> pure a
        Left p -> die [ "Internal error. Did not get transitive dependencies for package: " <> PackageName.print p.package.name ]

  let buildingMultiplePackages = Array.length selectedPackages > 1

  when buildingMultiplePackages do
    logInfo
      $ append "Building packages in the following order:\n"
      $ Array.intercalate "\n"
      $ Array.mapWithIndex (\i (Tuple p _) -> show (i + 1) <> ") " <> PackageName.print p.package.name) selectedPackages

  duplicateModuleRef <- liftEffect $ Ref.new Map.empty
  for_ selectedPackages \(Tuple selected allDependencies) -> do
    when buildingMultiplePackages do
      logInfo $ "Building package: " <> PackageName.print selected.package.name
    let
      globs = getBuildGlobs
        { dependencies: allDependencies
        , depsOnly: opts.depsOnly
        , withTests: true
        , selected: SinglePackageGlobs selected
        }
    depPathDecisions <- liftEffect $ sequence $ Psa.toPathDecisions
      { allDependencies
      , psaCliFlags
      , workspaceOptions:
          { censorLibWarnings: workspace.buildOptions.censorLibWarnings
          , censorLibCodes: workspace.buildOptions.censorLibCodes
          , filterLibCodes: workspace.buildOptions.filterLibCodes
          }
      }
    projectPathDecision <- liftEffect $ Psa.toWorkspacePackagePathDecision
      { selected
      , psaCliFlags
      }
    let
      psaArgs =
        { color: logOptions.color
        , jsonErrors: opts.jsonErrors
        , decisions: projectPathDecision <> join depPathDecisions
        , statVerbosity: fromMaybe Psa.defaultStatVerbosity workspace.buildOptions.statVerbosity
        }

    Psa.psaCompile globs args psaArgs

    case workspace.backend of
      Nothing -> pure unit
      Just backend -> do
        logInfo $ "Compiling with backend \"" <> backend.cmd <> "\""
        logDebug $ "Running command `" <> backend.cmd <> "`"
        let
          moreBackendArgs = case backend.args of
            Just as | Array.length as > 0 -> as
            _ -> []
        Cmd.exec backend.cmd (addOutputArgs moreBackendArgs) Cmd.defaultExecOptions >>= case _ of
          Left err -> do
            logDebug $ show err
            die [ "Failed to build with backend " <> backend.cmd ]
          Right _r ->
            logSuccess "Backend build succeeded."

    if buildingMultiplePackages then do
      logDebug $ "Getting purs graph using globs for package: " <> PackageName.print selected.package.name
      maybeGraph <- Graph.runGraph globs opts.pursArgs
      case maybeGraph of
        Nothing ->
          logWarn $
            "Due to JSON decoding failure on 'purs graph' output, \
            \packages that define modules with the same name might not be detected."
        Just graph -> do
          let
            toModulesDefinedByThisPackage :: Purs.ModuleGraphNode -> Maybe (Array (Tuple PackageName FilePath))
            toModulesDefinedByThisPackage v =
              [ Tuple selected.package.name v.path ] <$ (String.stripPrefix (String.Pattern selected.path) v.path)
            modulesDefinedByThisPackage = Map.mapMaybe toModulesDefinedByThisPackage $ unwrap graph

          liftEffect $ Ref.modify_ (Map.unionWith (<>) modulesDefinedByThisPackage) duplicateModuleRef
          when workspace.buildOptions.pedanticPackages do
            logInfo $ "Looking for unused and undeclared transitive dependencies..."
            env <- ask
            errors <- Graph.toImportErrors selected <$> runSpago (Record.union { graph, selected } env) Graph.checkImports
            unless (Array.null errors) do
              die' errors

    else do
      when workspace.buildOptions.pedanticPackages do
        logInfo $ "Looking for unused and undeclared transitive dependencies..."
        maybeGraph <- Graph.runGraph globs opts.pursArgs
        for_ maybeGraph \graph -> do
          env <- ask
          errors <- Graph.toImportErrors selected <$> runSpago (Record.union { graph, selected } env) Graph.checkImports
          unless (Array.null errors) do
            die' errors

  when buildingMultiplePackages do
    moduleMap <- liftEffect $ Ref.read duplicateModuleRef
    let duplicateModules = Map.filter (\packages -> Array.length packages > 1) moduleMap
    unless (Map.isEmpty duplicateModules) do
      die $ duplicateModulesError duplicateModules

-- TODO: if we are building with all the packages (i.e. selected = Nothing),
-- then we can use the graph to remove outdated modules from `output`!

duplicateModulesError :: Map String (Array (Tuple PackageName FilePath)) -> Docc
duplicateModulesError duplicateModules =
  Dodo.lines
    [ Dodo.text $ "Detected " <> show (Map.size duplicateModules) <> " modules with the same module name across 2 or more packages defined in this workspace."
    , duplicateModules
        # Map.toUnfoldable
        # Array.mapWithIndex
            ( \idx (Tuple m ps) ->
                Dodo.lines
                  [ Dodo.text $ show (idx + 1) <> ") Module \"" <> m <> "\" was defined in the following packages:"
                  , Dodo.indent
                      $ Dodo.lines
                      $ map
                          ( \(Tuple pkg moduleFilePath) ->
                              Dodo.text $ "- " <> PackageName.print pkg <> "   at path: " <> moduleFilePath
                          )
                      $ Array.sort ps
                  ]
            )
        # Dodo.lines
    ] :: Docc

data SelectedPackageGlob
  = SinglePackageGlobs WorkspacePackage
  | AllWorkspaceGlobs PackageSet

type BuildGlobsOptions =
  { withTests :: Boolean
  , depsOnly :: Boolean
  , selected :: SelectedPackageGlob
  , dependencies :: PackageMap
  }

getBuildGlobs :: BuildGlobsOptions -> Set FilePath
getBuildGlobs { selected, dependencies, withTests, depsOnly } =
  Set.fromFoldable $ projectGlobs <> monorepoPkgGlobs <> dependencyGlobs <> [ BuildInfo.buildInfoPath ]
  where
  testGlobs = case withTests of
    true -> WithTestGlobs
    false -> NoTestGlobs

  workspacePackageGlob :: WorkspacePackage -> Array String
  workspacePackageGlob p = Config.sourceGlob testGlobs p.package.name (WorkspacePackage p)

  -- Note: `depsOnly` means "no packages from the monorepo", so we filter out the workspace packages.
  -- See its usage again in `monorepoPkgGlobs`.
  { projectGlobs, monorepoTestGlobs } = case depsOnly of
    true ->
      { projectGlobs: []
      , monorepoTestGlobs: testGlobs
      }
    false -> case selected of
      SinglePackageGlobs selectedPackage ->
        { projectGlobs: workspacePackageGlob selectedPackage
        , monorepoTestGlobs: NoTestGlobs
        }
      AllWorkspaceGlobs packageSet ->
        { projectGlobs: workspacePackageGlob =<< Config.getWorkspacePackages packageSet
        , monorepoTestGlobs: testGlobs
        }

  { yes: monorepoPkgs, no: dependencyPkgs } = partition isWorkspacePackage $ Map.toUnfoldable dependencies

  dependencyGlobs = (Tuple.uncurry $ Config.sourceGlob NoTestGlobs) =<< dependencyPkgs
  monorepoPkgGlobs
    | depsOnly = []
    | otherwise = (Tuple.uncurry $ Config.sourceGlob monorepoTestGlobs) =<< monorepoPkgs

isWorkspacePackage :: Tuple PackageName Package -> Boolean
isWorkspacePackage = case _ of
  Tuple _ (WorkspacePackage _) -> true
  _ -> false
