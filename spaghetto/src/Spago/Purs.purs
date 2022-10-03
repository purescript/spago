module Spago.Purs where

import Spago.Prelude

import Data.Set as Set

type PursEnv a =
  { purs :: FilePath
  , logOptions :: LogOptions
  | a
  }

compile :: forall a. Set FilePath -> Array String -> Spago (PursEnv a) Unit
compile globs pursArgs = do
  { purs } <- ask
  let args = [ "compile" ] <> pursArgs <> Set.toUnfoldable globs
  logDebug [ "Running command: purs", "With args: " <> show args ]
  void $ liftAff $ spawnFromParentWithStdin
    { command: purs
    , args
    , input: Nothing
    , cwd: Nothing
    }
  logSuccess "Build succeeded."
