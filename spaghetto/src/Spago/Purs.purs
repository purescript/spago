module Spago.Purs where

import Spago.Prelude

import Data.Set as Set
import Spago.Cmd as Cmd

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
  liftAff (Cmd.exec purs args Cmd.defaultExecOptions) >>= case _ of
    Right _r -> logSuccess "Build succeeded."
    Left err -> do
      logDebug $ show err
      die [ "Failed to build." ]
