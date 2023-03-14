module Spago.Command.Build
  ( run
  , BuildEnv
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Tuple as Tuple
import Spago.BuildInfo as BuildInfo
import Spago.Cmd as Cmd
import Spago.Config (Package(..), WithTestGlobs(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Git (Git)
import Spago.Purs (Purs)
import Spago.Purs as Purs
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
  }

run :: forall a. BuildOptions -> Spago (BuildEnv a) Unit
run opts = do
  logInfo "Building..."
  { dependencies, workspace } <- ask
  let
    -- depsOnly means "no packages from the monorepo", so we filter out the workspace packages
    dependencyGlobs = map (Tuple.uncurry $ Config.sourceGlob WithTestGlobs) case opts.depsOnly of
      false -> Map.toUnfoldable dependencies
      true -> Map.toUnfoldable $ Map.filter
        ( case _ of
            WorkspacePackage _ -> false
            _ -> true
        )
        dependencies

    -- Here we select the right globs for a monorepo setup with a bunch of packages
    projectSources =
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

  let
    buildBackend globs = do
      case workspace.backend of
        Nothing ->
          Purs.compile globs (addOutputArgs opts.pursArgs)
        Just backend -> do
          when (isJust $ Cmd.findFlag { flags: [ "-g", "--codegen" ], args: opts.pursArgs }) do
            die
              [ "Can't pass `--codegen` option to build when using a backend"
              , "Hint: No need to pass `--codegen corefn` explicitly when using the `backend` option."
              , "Remove the argument to solve the error"
              ]
          Purs.compile globs $ (addOutputArgs opts.pursArgs) <> [ "--codegen", "corefn" ]

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

  let globs = Set.fromFoldable $ join projectSources <> join dependencyGlobs <> [ BuildInfo.buildInfoPath ]
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
