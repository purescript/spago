module Spago.Esbuild (Esbuild, getEsbuild) where

import Spago.Prelude

import Spago.Cmd as Cmd

type Esbuild =
  { cmd :: FilePath
  , version :: String 
  }

getEsbuild :: forall a. Spago (LogEnv a) Esbuild
getEsbuild = 
  Cmd.exec "esbuild" ["--version"] Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false } >>= case _ of
    Left err -> do
      logDebug $ show err
      die [ "Failed to find esbuild. See https://esbuild.github.io/getting-started/#install-esbuild for ways to install esbuild." ] 
    Right r -> pure { cmd: "esbuild", version: r.stdout }
