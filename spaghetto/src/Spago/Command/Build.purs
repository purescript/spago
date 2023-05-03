module Spago.Command.Build
  ( run
  , BuildEnv
  ) where

import Spago.Prelude

import Control.Alternative as Alternative
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.Tuple as Tuple
import Registry.PackageName as PackageName
import Spago.BuildInfo as BuildInfo
import Spago.Cmd as Cmd
import Spago.Config (Package(..), WithTestGlobs(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Config as Core
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
  , censorBuildWarnings :: Maybe Core.CensorBuildWarnings
  , censorCodes :: Maybe (NonEmptySet String)
  , filterCodes :: Maybe (NonEmptySet String)
  , statVerbosity :: Maybe Core.StatVerbosity
  , showSource :: Maybe Core.ShowSourceCode
  , strict :: Maybe Boolean
  , persistWarnings :: Maybe Boolean
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
  , censorBuildWarnings
  , censorCodes
  , filterCodes
  , statVerbosity
  , showSource
  , strict
  , persistWarnings
  } <- ask
  let
    isWorkspacePackage = case _ of
      Tuple _ (WorkspacePackage _) -> true
      _ -> false

    { yes: monorepoPkgs, no: dependencyPkgs } = partition isWorkspacePackage $ Map.toUnfoldable dependencies
    -- depsOnly means "no packages from the monorepo", so we filter out the workspace packages
    dependencyGlobs = (Tuple.uncurry $ Config.sourceGlob WithTestGlobs) =<< dependencyPkgs
    monorepoPkgGlobs
      | opts.depsOnly = []
      | otherwise = (Tuple.uncurry $ Config.sourceGlob WithTestGlobs) =<< monorepoPkgs

    dependencyLibs = map (Tuple.uncurry Config.getPackageLocation) dependencyPkgs

    -- Here we select the right globs for a monorepo setup with a bunch of packages
    projectSources = join
      if opts.depsOnly then []
      else case workspace.selected of
        Just p -> [ workspacePackageGlob p ]
        -- We just select all the workspace package globs, because it's (1) intuitive and (2) backwards compatible
        Nothing -> map workspacePackageGlob (Config.getWorkspacePackages workspace.packageSet)
  logDebug $ "Project sources: " <> show projectSources

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
      { strict: fromMaybe Psa.defaultParseOptions.strict strict
      , censorBuildWarnings: fromMaybe Psa.defaultParseOptions.censorBuildWarnings censorBuildWarnings
      , showSource: fromMaybe Psa.defaultParseOptions.showSource showSource
      , censorCodes: maybe Psa.defaultParseOptions.censorCodes NonEmptySet.toSet censorCodes
      , filterCodes: maybe Psa.defaultParseOptions.filterCodes NonEmptySet.toSet filterCodes
      , statVerbosity: fromMaybe Psa.defaultParseOptions.statVerbosity statVerbosity
      , stashFile: do
          Alternative.guard (not opts.depsOnly)
          shouldStashWarnings <- persistWarnings
          Alternative.guard shouldStashWarnings
          case workspace.selected of
            Just p -> Just $ Paths.mkLocalCachesPersistentWarningsFile $ PackageName.print p.package.name
            Nothing -> Just Paths.localCachesPersistedWarningsEntireWorkspace
      }
    buildBackend globs = do
      case workspace.backend of
        Nothing ->
          Psa.psaCompile globs (addOutputArgs opts.pursArgs) psaArgs psaOptions
        Just backend -> do
          when (isJust $ Cmd.findFlag { flags: [ "-g", "--codegen" ], args: opts.pursArgs }) do
            die
              [ "Can't pass `--codegen` option to build when using a backend"
              , "Hint: No need to pass `--codegen corefn` explicitly when using the `backend` option."
              , "Remove the argument to solve the error"
              ]
          let args = (addOutputArgs opts.pursArgs) <> [ "--codegen", "corefn" ]
          Psa.psaCompile globs args psaArgs psaOptions

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

  let globs = Set.fromFoldable $ projectSources <> monorepoPkgGlobs <> dependencyGlobs <> [ BuildInfo.buildInfoPath ]
  buildBackend globs

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

  where

  workspacePackageGlob :: WorkspacePackage -> Array String
  workspacePackageGlob p = Config.sourceGlob WithTestGlobs p.package.name (WorkspacePackage p)
