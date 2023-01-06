module Spago.Command.Build
  ( run
  , BuildEnv
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Tuple as Tuple
import Registry.PackageName (PackageName)
import Spago.BuildInfo as BuildInfo
import Spago.Cmd as Cmd
import Spago.Config (Package(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Git (Git)
import Spago.Purs (Purs)
import Spago.Purs as Purs

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
  let dependencyGlobs = map (Tuple.uncurry Config.sourceGlob) (Map.toUnfoldable dependencies)

  -- Here we select the right globs for a monorepo setup
  let
    workspacePackageGlob :: WorkspacePackage -> Array String
    workspacePackageGlob p = Config.sourceGlob p.package.name (WorkspacePackage p)

    -- TODO: here depsOnly means "no packages from the monorepo", but right now we include local dependencies from the monorepo
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
    addOutputArgs args = case workspace.output of
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
            Right _r -> logSuccess "Backend build succeeded."
            Left err -> do
              logDebug $ show err
              die [ "Failed to build with backend " <> backend.cmd ]

  {-
  TODO: before, then, else
      buildAction globs = do
        let action = buildBackend globs >> (fromMaybe (pure ()) maybePostBuild)
        runCommands "Before" beforeCommands
        action `onException` (runCommands "Else" elseCommands)
        runCommands "Then" thenCommands
  -}

  buildBackend (Set.fromFoldable $ join projectSources <> join dependencyGlobs <> [ BuildInfo.buildInfoPath ])

