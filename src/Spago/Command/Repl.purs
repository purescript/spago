module Spago.Command.Repl
  ( run
  , ReplEnv
  ) where

import Spago.Prelude

import Data.Map as Map
import Spago.Command.Build as Build
import Spago.Config (PackageMap)
import Spago.Purs (Purs)
import Spago.Purs as Purs

type ReplEnv a =
  { purs :: Purs
  , packageDependencies :: Map PackageName PackageMap
  , supportPackage :: PackageMap
  , depsOnly :: Boolean
  , logOptions :: LogOptions
  , pursArgs :: Array String
  , selected :: Build.SelectedPackageGlob
  | a
  }

run :: forall a. Spago (ReplEnv a) Unit
run = do
  { packageDependencies, purs, logOptions, pursArgs, selected, depsOnly, supportPackage } <- ask

  let
    dependencies = foldl (Map.unionWith (\l _ -> l)) supportPackage packageDependencies
    globs = Build.getBuildGlobs
      { selected
      , dependencies
      , depsOnly
      , withTests: true
      }
  void $ runSpago { purs, logOptions } $ Purs.repl globs pursArgs
