module Spago.Command.Repl
  ( run
  , ReplEnv
  , supportPackage
  ) where

import Spago.Prelude

import Data.Map as Map
import Registry.PackageName as PackageName
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
  , selected :: Array WorkspacePackage
  | a
  }

run :: forall a. Spago (ReplEnv a) Unit
run = do
  { dependencies, purs, logOptions, pursArgs, selected, depsOnly } <- ask

  let
    globs = Build.getBuildGlobs
      { selected
      , dependencies
      , depsOnly
      , withTests: true
      }
  void $ runSpago { purs, logOptions } $ Purs.repl globs pursArgs

-- TODO I guess this should be configurable
supportPackageName :: PackageName
supportPackageName = unsafeFromRight $ PackageName.parse "psci-support"

supportPackage :: Map PackageName Package -> Map PackageName Package
supportPackage packageSet = Map.filterWithKey (\k _v -> k == supportPackageName) packageSet
