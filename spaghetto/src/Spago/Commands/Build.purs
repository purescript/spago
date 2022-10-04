module Spago.Command.Build where

import Spago.Prelude

import Data.Map as Map
import Data.Set as Set
import Data.Tuple as Tuple
import Registry.PackageName (PackageName)
import Spago.BuildInfo as BuildInfo
import Spago.Cmd as Cmd
import Spago.Config (Package(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Purs as Purs

type BuildEnv a =
  { purs :: FilePath
  , git :: FilePath
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
  void $ liftAff $ spawnFromParentWithStdin { command: "purs", args: [ "--version" ], input: Nothing, cwd: Nothing }
  { dependencies, workspace } <- ask
  let dependencyGlobs = map (Tuple.uncurry Config.sourceGlob) (Map.toUnfoldable dependencies)

  -- Here we select the right globs for a monorepo setup
  let
    workspacePackageGlob :: WorkspacePackage -> String
    workspacePackageGlob p = Config.sourceGlob p.package.name (WorkspacePackage p)
    projectSources =
      if opts.depsOnly then []
      else case workspace.selected of
        Just p -> [ workspacePackageGlob p ]
        -- We just select all the workspace package globs, because it's (1) intuitive and (2) backwards compatible
        Nothing -> map workspacePackageGlob (Config.getWorkspacePackages workspace.packageSet)
  logDebug $ "Project sources: " <> show projectSources

  BuildInfo.writeBuildInfo

  let
    buildBackend globs = do
      case workspace.backend of
        Nothing ->
          Purs.compile globs opts.pursArgs
        Just backend -> do
          when (isJust $ Cmd.findFlag { flags: [ "-g", "--codegen" ], args: opts.pursArgs }) do
            die
              [ "Can't pass `--codegen` option to build when using a backend"
              , "Hint: No need to pass `--codegen corefn` explicitly when using the `backend` option."
              , "Remove the argument to solve the error"
              ]
          Purs.compile globs $ opts.pursArgs <> [ "--codegen", "corefn" ]

          logDebug $ "Compiling with backend \"" <> backend <> "\""
          let backendCmd = backend -- TODO: we might want to pass args to the backend here
          logDebug $ "Running command `" <> backendCmd <> "`"
          void $ liftAff $ spawnFromParentWithStdin
            { command: backendCmd
            , args: []
            , input: Nothing
            , cwd: Nothing
            }
          logSuccess "Backend build succeeded."

  {-
  TODO:
      buildAction globs = do
        let action = buildBackend globs >> (fromMaybe (pure ()) maybePostBuild)
        runCommands "Before" beforeCommands
        action `onException` (runCommands "Else" elseCommands)
        runCommands "Then" thenCommands
  -}

  buildBackend (Set.fromFoldable $ projectSources <> dependencyGlobs <> [ BuildInfo.buildInfoPath ])

