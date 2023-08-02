module Spago.Command.Build
  ( run
  , BuildEnv
  , getBuildGlobs
  ) where

import Spago.Prelude

import Control.Alternative as Alternative
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.Tuple as Tuple
import Registry.PackageName as PackageName
import Spago.BuildInfo as BuildInfo
import Spago.Cmd as Cmd
import Spago.Config (Package(..), WithTestGlobs(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Git (Git)
import Spago.Paths as Paths
import Spago.Psa as Psa
import Spago.Purs (Purs)
import Spago.Purs.Graph as Graph

type BuildEnv a =
  { purs :: Purs
  , git :: Git
  , dependencies :: Map PackageName Package
  , logOptions :: LogOptions
  , workspace :: Workspace
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
  } <- ask
  let
    { yes: _monorepoPkgs, no: dependencyPkgs } = partition isWorkspacePackage $ Map.toUnfoldable dependencies
    dependencyLibs = map (Tuple.uncurry Config.getPackageLocation) dependencyPkgs

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

  let
    psaArgs =
      { libraryDirs: dependencyLibs
      , color: logOptions.color
      , jsonErrors: opts.jsonErrors
      }
    psaOptions =
      { strict: fromMaybe Psa.defaultParseOptions.strict workspace.buildOptions.strict
      , censorBuildWarnings: fromMaybe Psa.defaultParseOptions.censorBuildWarnings workspace.buildOptions.censorBuildWarnings
      , showSource: fromMaybe Psa.defaultParseOptions.showSource workspace.buildOptions.showSource
      , censorCodes: maybe Psa.defaultParseOptions.censorCodes NonEmptySet.toSet workspace.buildOptions.censorCodes
      , filterCodes: maybe Psa.defaultParseOptions.filterCodes NonEmptySet.toSet workspace.buildOptions.filterCodes
      , statVerbosity: fromMaybe Psa.defaultParseOptions.statVerbosity workspace.buildOptions.statVerbosity
      , stashFile: do
          Alternative.guard (not opts.depsOnly)
          shouldStashWarnings <- workspace.buildOptions.persistWarnings
          Alternative.guard shouldStashWarnings
          case workspace.selected of
            Just p -> Just $ Paths.mkLocalCachesPersistentWarningsFile $ PackageName.print p.package.name
            Nothing -> Just Paths.localCachesPersistedWarningsEntireWorkspace
      }
    buildBackend = do
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

  {-
  TODO: before, then, else
      buildAction globs = do
        let action = buildBackend globs >> (fromMaybe (pure ()) maybePostBuild)
        runCommands "Before" beforeCommands
        action `onException` (runCommands "Else" elseCommands)
        runCommands "Then" thenCommands
  -}

  let
    globs = getBuildGlobs
      { dependencies
      , depsOnly: opts.depsOnly
      , withTests: true
      , selected: case workspace.selected of
          Just p -> [ p ]
          Nothing -> Config.getWorkspacePackages workspace.packageSet
      }

  when (isJust $ Cmd.findFlag { flags: [ "-g", "--codegen" ], args: opts.pursArgs }) do
    die
      [ "Can't pass `--codegen` option to build when using a backend"
      , "Hint: No need to pass `--codegen corefn` explicitly when using the `backend` option."
      , "Remove the argument to solve the error"
      ]
  let args = (addOutputArgs opts.pursArgs) <> [ "--codegen", "corefn,docs,js,sourcemaps" ]
  Psa.psaCompile globs args psaArgs psaOptions

  buildBackend

  when workspace.buildOptions.pedanticPackages do
    logInfo $ "Looking for unused and undeclared transitive dependencies..."
    errors <- case workspace.selected of
      Just selected -> Graph.runGraphCheck selected globs opts.pursArgs
      Nothing -> do
        -- TODO: here we could go through all the workspace packages and run the check for each
        -- The complication is that "dependencies" includes all the dependencies for all packages
        map Array.fold $ for (Config.getWorkspacePackages workspace.packageSet) \selected -> do
          Graph.runGraphCheck selected globs opts.pursArgs
    unless (Array.null errors) do
      die' errors

-- TODO: if we are building with all the packages (i.e. selected = Nothing),
-- then we can use the graph to remove outdated modules from `output`!

type BuildGlobsOptions =
  { withTests :: Boolean
  , depsOnly :: Boolean
  , selected :: Array WorkspacePackage
  , dependencies :: Map PackageName Package
  }

getBuildGlobs :: BuildGlobsOptions -> Set FilePath
getBuildGlobs { selected, dependencies, withTests, depsOnly } =
  Set.fromFoldable $ projectGlobs <> monorepoPkgGlobs <> dependencyGlobs <> [ BuildInfo.buildInfoPath ]
  where
  -- Here we select the right globs for a monorepo setup with a bunch of packages
  projectGlobs = join case depsOnly of
    true -> []
    false ->
      -- We just select all the workspace package globs, because it's (1) intuitive and (2) backwards compatible
      map workspacePackageGlob selected

  testGlobs = case withTests of
    true -> WithTestGlobs
    false -> NoTestGlobs

  workspacePackageGlob :: WorkspacePackage -> Array String
  workspacePackageGlob p = Config.sourceGlob testGlobs p.package.name (WorkspacePackage p)

  { yes: monorepoPkgs, no: dependencyPkgs } = partition isWorkspacePackage $ Map.toUnfoldable dependencies
  -- depsOnly means "no packages from the monorepo", so we filter out the workspace packages
  dependencyGlobs = (Tuple.uncurry $ Config.sourceGlob NoTestGlobs) =<< dependencyPkgs
  monorepoPkgGlobs
    | depsOnly = []
    | otherwise = (Tuple.uncurry $ Config.sourceGlob testGlobs) =<< monorepoPkgs

isWorkspacePackage :: Tuple PackageName Package -> Boolean
isWorkspacePackage = case _ of
  Tuple _ (WorkspacePackage _) -> true
  _ -> false
