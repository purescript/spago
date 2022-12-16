module Spago.Command.Bundle where

import Spago.Prelude

import Node.Path as Path
import Spago.Cmd as Cmd
import Spago.Config (BundlePlatform(..), BundleType(..), Workspace, WorkspacePackage)

type BundleEnv a =
  { esbuild :: FilePath
  , logOptions :: LogOptions
  , bundleOptions :: BundleOptions
  , workspace :: Workspace
  , selected :: WorkspacePackage
  | a
  }

type BundleOptions =
  { minify :: Boolean
  , module :: String
  , outfile :: FilePath
  , platform :: BundlePlatform
  , type :: BundleType
  }

type RawBundleOptions =
  { minify :: Boolean
  , module :: String
  , outfile :: FilePath
  , platform :: String
  , type :: String
  }

run :: forall a. Spago (BundleEnv a) Unit
run = do
  { esbuild, selected, workspace, bundleOptions: opts } <- ask
  logDebug $ "Bundle options: " <> show opts
  let
    command = esbuild
    minify = if opts.minify then [ "--minify" ] else []
    outfile = Path.concat [ selected.path, opts.outfile ]
    format = case opts.platform, opts.type of
      BundleBrowser, BundleApp -> "--format=iife"
      _, _ -> "--format=esm"

    -- See https://github.com/evanw/esbuild/issues/1921
    nodePatch = case opts.platform of
      BundleNode -> [ "--banner:js=import __module from \'module\';import __path from \'path\';import __url from \'url\';const require = __module.createRequire(import.meta.url);" ]
      _ -> []

    output = case workspace.output of
      Nothing -> "output"
      Just o -> o
    -- TODO: we might need to use `Path.relative selected.path output` instead of just output there
    mainPath = Path.concat [ output, opts.module, "index.js" ]

    { input, entrypoint } = case opts.type of
      BundleApp -> { entrypoint: [], input: Cmd.StdinWrite ("#!/usr/bin/env node\n\nimport { main } from './" <> mainPath <> "'; main();") }
      BundleModule -> { entrypoint: [ mainPath ], input: Cmd.StdinNewPipe }
    execOptions = Cmd.defaultExecOptions { pipeStdin = input }

    args =
      [ "--bundle"
      , "--outfile=" <> outfile
      , "--platform=" <> show opts.platform
      , format
      ] <> minify <> entrypoint <> nodePatch
  logInfo "Bundling..."
  logDebug $ "Running esbuild: " <> show args
  Cmd.exec command args execOptions >>= case _ of
    Right _r -> logSuccess "Bundle succeeded."
    Left err -> do
      logDebug $ show err
      die [ "Failed to bundle." ]
