module Spago.Command.Build where

import Spago.Prelude

import Data.Map as Map
import Data.Tuple as Tuple
import Registry.PackageName (PackageName)
import Spago.Config (Package(..), Workspace, WorkspacePackage)
import Spago.Config as Config

type BuildEnv a =
  { purs :: FilePath
  , git :: FilePath
  , dependencies :: Map PackageName Package
  , logOptions :: LogOptions
  , workspace :: Workspace
  | a
  }

type BuildOptions =
  { depsOnly :: Boolean }

run :: forall a. BuildOptions -> Spago (BuildEnv a) Unit
run opts = do
  logInfo "Building..."
  void $ liftAff $ spawnFromParentWithStdin { command: "purs", args: [ "--version" ], input: Nothing, cwd: Nothing }
  { purs, dependencies, workspace } <- ask
  let command = purs
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

  let args = [ "compile" ] <> projectSources <> dependencyGlobs
  logDebug [ "Running command: purs", "With args: " <> show args ]
  void $ liftAff $ spawnFromParentWithStdin
    { command
    , args
    , input: Nothing
    , cwd: Nothing
    }
  logSuccess "Build succeeded."
