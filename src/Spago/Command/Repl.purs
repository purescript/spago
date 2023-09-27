module Spago.Command.Repl
  ( run
  , ReplEnv
  ) where

import Spago.Prelude

import Spago.Command.Build as Build
import Spago.Config (Package, WorkspacePackage)
import Spago.Purs (Purs)
import Spago.Purs as Purs

type ReplEnv a =
  { purs :: Purs
  , dependencies :: Map PackageName Package
  , depsOnly :: Boolean
  , logOptions :: LogOptions
  , pursArgs :: Array String
  , selected :: Either (Array WorkspacePackage) WorkspacePackage
  | a
  }

run :: forall a. Spago (ReplEnv a) Unit
run = do
  { dependencies, purs, logOptions, pursArgs, selected, depsOnly } <- ask

  let
    globs = case selected of
      Right selectedPkg ->
        Build.getBuildGlobs
          { selected: selectedPkg
          , dependencies
          , depsOnly
          , withTests: true
          }
      Left workspacePackages ->
        Build.getEntireWorkspaceGlobs
          { workspacePackages
          , dependencies
          , depsOnly
          , withTests: true
          }
  void $ runSpago { purs, logOptions } $ Purs.repl globs pursArgs
