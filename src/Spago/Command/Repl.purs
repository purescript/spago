module Spago.Command.Repl
  ( run
  , ReplEnv
  ) where

import Spago.Prelude

import Data.Map as Map
import Spago.Command.Build as Build
import Spago.Command.Fetch as Fetch
import Spago.Command.Init (pursReplFile)
import Spago.Config (PackageMap, WorkspacePackage)
import Spago.FS as FS
import Spago.Purs (Purs)
import Spago.Purs as Purs

type ReplEnv a =
  { purs :: Purs
  , dependencies :: Fetch.PackageTransitiveDeps
  , supportPackage :: PackageMap
  , depsOnly :: Boolean
  , logOptions :: LogOptions
  , rootPath :: RootPath
  , pursArgs :: Array String
  , selected :: NonEmptyArray WorkspacePackage
  | a
  }

run :: ∀ a. Spago (ReplEnv a) Unit
run = do
  { rootPath, dependencies, pursArgs, selected, depsOnly, supportPackage } <- ask

  let replFile = rootPath </> pursReplFile.name
  unlessM (FS.exists replFile) $
    FS.writeTextFile replFile pursReplFile.content

  let
    allDependencies = Map.unionWith (\l _ -> l) supportPackage $ Fetch.toAllDependencies dependencies
    globs = Build.getBuildGlobs
      { rootPath
      , selected
      , dependencies: allDependencies
      , depsOnly
      , withTests: true
      }
  void $ Purs.repl rootPath globs pursArgs
