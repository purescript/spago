module Spago.Command.Build where

import Spago.Prelude

import Data.Map as Map
import Data.Tuple as Tuple
import Registry.PackageName (PackageName)
import Spago.Config (Package)
import Spago.Config as Config

type BuildEnv a =
  { purs :: FilePath
  , git :: FilePath
  , dependencies :: Map PackageName Package
  , logOptions :: LogOptions
  | a
  }

type BuildOptions =
  { depsOnly :: Boolean }

run :: forall a. BuildOptions -> Spago (BuildEnv a) Unit
run opts = do
  logInfo "Building..."
  void $ liftAff $ spawnFromParentWithStdin { command: "purs", args: [ "--version" ], input: Nothing, cwd: Nothing }
  { purs, dependencies } <- ask
  let command = purs
  let dependencyGlobs = map (Tuple.uncurry Config.sourceGlob) (Map.toUnfoldable dependencies)
  -- TODO: here we can select the right glob for a monorepo setup
  let projectSources = if opts.depsOnly then [] else [ "src/**/*.purs" ]
  let args = [ "compile" ] <> projectSources <> dependencyGlobs
  logDebug [ "Running command: purs", "With args: " <> show args ]
  void $ liftAff $ spawnFromParentWithStdin
    { command
    , args
    , input: Nothing
    , cwd: Nothing
    }
  logSuccess "Build succeeded."
