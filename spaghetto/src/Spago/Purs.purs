module Spago.Purs where

import Spago.Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.String as String
import Registry.Version (Version)
import Registry.Version as Version
import Spago.Cmd as Cmd

type PursEnv a =
  { purs :: Purs
  , logOptions :: LogOptions
  | a
  }

type Purs =
  { cmd :: FilePath
  , version :: Version
  }

compile :: forall a. Set FilePath -> Array String -> Spago (PursEnv a) Unit
compile globs pursArgs = do
  { purs } <- ask
  let args = [ "compile" ] <> pursArgs <> Set.toUnfoldable globs
  logDebug [ "Running command: purs", "With args: " <> show args ]
  Cmd.exec purs.cmd args Cmd.defaultExecOptions >>= case _ of
    Right _r -> logSuccess "Build succeeded."
    Left err -> do
      logDebug $ show err
      die [ "Failed to build." ]

getPurs :: forall a. Spago (LogEnv a) Purs
getPurs =
  Cmd.exec "purs" [ "--version" ] Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false } >>= case _ of
    Left err -> do
      logDebug $ show err
      die [ "Failed to find purs. Have you installed it, and is it in your PATH?" ]
    -- Drop the stuff after a space: dev builds look like this: 0.15.6 [development build; commit: 8da7e96005f717f03d6eee3c12b1f1416659a919]
    Right r -> case Version.parseVersion Version.Lenient (fromMaybe "" (Array.head (String.split (String.Pattern " ") r.stdout))) of
      Left _err -> die $ "Failed to parse purs version. Was: " <> r.stdout
      -- Fail if Purs is lower than 0.15.4
      Right v ->
        if Version.minor v >= 15 && Version.patch v >= 4 then
          pure { cmd: "purs", version: v }
        else
          die [ "Unsupported PureScript version " <> Version.printVersion v, "Please install PureScript v0.15.4 or higher." ]
