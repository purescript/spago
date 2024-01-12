module Spago.Command.Bundle where

import Spago.Prelude

import Node.Path as Path
import Spago.Cmd as Cmd
import Spago.Config (BundlePlatform(..), BundleType(..), Workspace, WorkspacePackage)
import Spago.Esbuild (Esbuild)

type BundleEnv a =
  { esbuild :: Esbuild
  , logOptions :: LogOptions
  , bundleOptions :: BundleOptions
  , workspace :: Workspace
  , selected :: WorkspacePackage
  | a
  }

type BundleOptions =
  { minify :: Boolean
  , sourceMaps :: Boolean
  , module :: String
  , outfile :: FilePath
  , platform :: BundlePlatform
  , type :: BundleType
  , extraArgs :: Array String
  }

type RawBundleOptions =
  { minify :: Boolean
  , module :: String
  , outfile :: FilePath
  , platform :: String
  , type :: String
  , extraArgs :: Array String
  }

run :: forall a. Spago (BundleEnv a) Unit
run = do
  { esbuild, selected, workspace, bundleOptions: opts } <- ask
  logDebug $ "Bundle options: " <> show opts
  let
    minify = if opts.minify then [ "--minify" ] else []
    sourceMap = if opts.sourceMaps then [ "--sourcemap" ] else []
    outfile = Path.concat [ selected.path, opts.outfile ]
    format = case opts.platform, opts.type of
      BundleBrowser, BundleApp -> "--format=iife"
      _, _ -> "--format=esm"

    -- See https://github.com/evanw/esbuild/issues/1921
    nodePatch = case opts.platform of
      BundleNode -> [ "--banner:js=import __module from \'module\';import __path from \'path\';import __url from \'url\';const require = __module.createRequire(import.meta.url);const __dirname = __path.dirname(__url.fileURLToPath(import.meta.url));const __filename=new URL(import.meta.url).pathname" ]
      _ -> []

    output = case workspace.buildOptions.output of
      Nothing -> "output"
      Just o -> o
    -- TODO: we might need to use `Path.relative selected.path output` instead of just output there
    mainPath = withForwardSlashes $ Path.concat [ output, opts.module, "index.js" ]

    shebang = case opts.platform of
      BundleNode -> "#!/usr/bin/env node\n\n"
      _ -> ""

    { input, entrypoint } = case opts.type of
      BundleApp -> { entrypoint: [], input: Cmd.StdinWrite (shebang <> "import { main } from './" <> mainPath <> "'; main();") }
      BundleModule -> { entrypoint: [ mainPath ], input: Cmd.StdinNewPipe }
    execOptions = Cmd.defaultExecOptions { pipeStdin = input }

    args =
      [ "--bundle"
      , "--outfile=" <> outfile
      , "--platform=" <> show opts.platform
      -- See https://github.com/evanw/esbuild/issues/1051
      , "--loader:.node=file"
      , format
      ] <> opts.extraArgs <> minify <> sourceMap <> entrypoint <> nodePatch
  logInfo "Bundling..."
  logDebug $ "Running esbuild: " <> show args
  Cmd.exec esbuild.cmd args execOptions >>= case _ of
    Right _ -> logSuccess "Bundle succeeded."
    Left r -> do
      logDebug $ Cmd.printExecResult r
      die [ "Failed to bundle." ]
