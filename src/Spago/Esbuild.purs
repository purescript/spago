module Spago.Esbuild (Esbuild, getEsbuild) where

import Spago.Prelude

import Spago.Cmd as Cmd

type Esbuild =
  { cmd :: GlobalPath
  , version :: String
  }

getEsbuild :: forall a. Spago (LogEnv a) Esbuild
getEsbuild = do
  { cmd, output } <- Cmd.getExecutable "esbuild"
  pure { cmd, version: output }
