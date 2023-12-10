module Spago.Command.Repl
  ( run
  , ReplEnv
  ) where

import Spago.Prelude

import Data.Map as Map
import Spago.Command.Build as Build
import Spago.Command.Fetch as Fetch
import Spago.Config (PackageMap, WorkspacePackage)
import Spago.Purs (Purs)
import Spago.Purs as Purs

type ReplEnv a =
  { purs :: Purs
  , dependencies :: Fetch.PackageTransitiveDeps
  , supportPackage :: PackageMap
  , depsOnly :: Boolean
  , logOptions :: LogOptions
  , pursArgs :: Array String
  , selected :: NonEmptyArray WorkspacePackage
  | a
  }

run :: Spago (ReplEnv _) Unit
run = do
  { dependencies, pursArgs, selected, depsOnly, supportPackage } <- ask

  let
    allDependencies = Map.unionWith (\l _ -> l) supportPackage $ Fetch.toAllDependencies dependencies
    globs = Build.getBuildGlobs
      { selected
      , dependencies: allDependencies
      , depsOnly
      , withTests: true
      }
  void $ Purs.repl globs pursArgs
