module Spago.Command.Bundle where

import Spago.Prelude

import Node.Path as Path
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Regex (regex, split)
import Data.String.Regex.Flags (global)
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
  , external :: Maybe String
  }

type RawBundleOptions =
  { minify :: Boolean
  , module :: String
  , outfile :: FilePath
  , platform :: String
  , type :: String
  , external :: Maybe String
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

    external = case opts.external of
      Nothing -> []
      Just ext -> map (\a -> "--external:" <> a) $ split (regex ",[ ]*" global) ext

    -- See https://github.com/evanw/esbuild/issues/1921
    nodePatch = case opts.platform of
      BundleNode -> [ "--banner:js=import __module from \'module\';import __path from \'path\';import __url from \'url\';const require = __module.createRequire(import.meta.url);" ]
      _ -> []

    output = case workspace.buildOptions.output of
      Nothing -> "output"
      Just o -> o
    -- TODO: we might need to use `Path.relative selected.path output` instead of just output there
    mainPath =
      String.replaceAll (Pattern "\\") (Replacement "/") $ Path.concat [ output, opts.module, "index.js" ]

    { input, entrypoint } = case opts.type of
      BundleApp -> { entrypoint: [], input: Cmd.StdinWrite ("#!/usr/bin/env node\n\nimport { main } from './" <> mainPath <> "'; main();") }
      BundleModule -> { entrypoint: [ mainPath ], input: Cmd.StdinNewPipe }
    execOptions = Cmd.defaultExecOptions { pipeStdin = input }

    args =
      [ "--bundle"
      , "--outfile=" <> outfile
      , "--platform=" <> show opts.platform
      , format
      ] <> minify <> entrypoint <> nodePatch <> external
  logDebug $ "mainPath=" <> mainPath
  logInfo "Bundling..."
  logDebug $ "Running esbuild: " <> show args
  Cmd.exec command args execOptions >>= case _ of
    Right _r -> logSuccess "Bundle succeeded."
    Left err -> do
      logDebug $ show err
      die [ "Failed to bundle." ]
