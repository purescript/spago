module Spago.Command.Build where

import Spago.Prelude

import Data.Map as Map
import Data.Tuple as Tuple
import Registry.PackageName (PackageName)
import Spago.PackageSet (Package)
import Spago.Paths as Paths

type BuildEnv a =
  { purs :: FilePath
  , git :: FilePath
  , dependencies :: Map PackageName Package
  | a
  }

type BuildOptions =
  { depsOnly :: Boolean }

run :: forall a. BuildOptions -> Spago (BuildEnv a) Unit
run opts = do
  void $ liftAff $ spawnFromParentWithStdin { command: "purs", args: [ "--version" ], input: Nothing, cwd: Nothing }
  { purs, dependencies } <- ask
  let command = purs
  let dependencyGlobs = map (Tuple.uncurry Paths.sourceGlob) (Map.toUnfoldable dependencies)
  -- TODO: here we can select the right glob for a monorepo setup
  let projectSources = if opts.depsOnly then [] else [ "src/**/*.purs" ]
  let args = [ "compile" ] <> projectSources <> dependencyGlobs
  log $ "Running purs: " <> show args
  result <- liftAff $ spawnFromParentWithStdin
    { command
    , args
    , input: Nothing
    , cwd: Nothing
    }
  logShow result
