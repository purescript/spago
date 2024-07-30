module Spago.Command.Bundle where

import Spago.Prelude

import Data.Array (all, fold, take)
import Data.String as Str
import Data.String.Utils (startsWith)
import Node.Path as Path
import Spago.Cmd as Cmd
import Spago.Config (BundlePlatform(..), BundleType(..), Workspace, WorkspacePackage)
import Spago.Esbuild (Esbuild)
import Spago.FS as FS
import Spago.Generated.BuildInfo as BuildInfo

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
  , force :: Boolean
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

run :: ∀ a. Spago (BundleEnv a) Unit
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

    onlyForNode s = case opts.platform of
      BundleNode -> s
      BundleBrowser -> ""

    output = workspace.buildOptions.output # fromMaybe "output"
    -- TODO: we might need to use `Path.relative selected.path output` instead of just output there
    mainPath = withForwardSlashes $ Path.concat [ output, opts.module, "index.js" ]

    { input, entrypoint } = case opts.type of
      BundleApp ->
        { entrypoint: []
        , input: Cmd.StdinWrite $ fold [ onlyForNode "#!/usr/bin/env node\n\n", "import { main } from './", mainPath, "';main();" ]
        }
      BundleModule ->
        { entrypoint: [ mainPath ]
        , input: Cmd.StdinNewPipe
        }

    execOptions = Cmd.defaultExecOptions { pipeStdin = input }

    banner = fold
      [ bundleWatermarkPrefix
      , BuildInfo.packages."spago-bin"
      , " */"
      , onlyForNode nodeTargetPolyfill
      ]

    args = fold
      [ [ "--bundle"
        , "--outfile=" <> outfile
        , "--platform=" <> show opts.platform
        , "--banner:js=" <> banner
        , "--loader:.node=file" -- See https://github.com/evanw/esbuild/issues/1051
        , format
        ]
      , opts.extraArgs
      , minify
      , sourceMap
      , entrypoint
      ]

  whenM (FS.exists checkWatermarkMarkerFileName)
    $ unless opts.force
    $ whenM (isNotSpagoGeneratedFile outfile)
    $ die [ "Target file " <> opts.outfile <> " was not previously generated by Spago. Use --force to overwrite anyway." ]

  logInfo "Bundling..."
  logDebug $ "Running esbuild: " <> show args
  Cmd.exec esbuild.cmd args execOptions >>= case _ of
    Right _ -> logSuccess "Bundle succeeded."
    Left r -> do
      logDebug $ Cmd.printExecResult r
      die [ "Failed to bundle." ]

isNotSpagoGeneratedFile :: ∀ a. String -> Spago (BundleEnv a) Boolean
isNotSpagoGeneratedFile path = do
  exists <- FS.exists path
  if not exists then
    pure false
  else
    -- The first line of the file could be the marker, or it could the shebang
    -- if the bundle was compiled for Node, in which case the marker will be the
    -- second line. So we check the first two lines.
    FS.readTextFile path
      <#> Str.split (Str.Pattern "\n")
      >>> take 2
      >>> all (not startsWith bundleWatermarkPrefix)

bundleWatermarkPrefix :: String
bundleWatermarkPrefix = "/* Generated by Spago v"

-- Presence of this file gates the watermark check.
--
-- If this file exists in the current directory, the Bundle command will check
-- if the target bundle file already exists and has the watermark in it, and if
-- it doesn't have the watermark, will refuse to overwrite it for fear of
-- overwriting a user-generated file.
--
-- We gate this check on the presence of this file so that the check is only
-- performed in a controlled context (such as integration tests), but doesn't
-- work for normal users. The idea is that the users who upgrade their Spago
-- aren't immediately met with a refusal to overwrite the bundle. Instead, Spago
-- will overwrite the bundle just fine, but now the bundle will have the
-- watermark in it. Then, after some time, after enough users have upgraded and
-- acquired the watermark in their bundles, we will remove this gating
-- mechanism, and watermark checking will start working for normal users.
checkWatermarkMarkerFileName :: String
checkWatermarkMarkerFileName = ".check-bundle-watermark"

-- A polyfill inserted when building for Node to work around this esbuild issue:
-- https://github.com/evanw/esbuild/issues/1921
nodeTargetPolyfill :: String
nodeTargetPolyfill = Str.joinWith ";"
  [ "import __module from 'module'"
  , "import __path from 'path'"
  , "import __url from 'url'"
  , "const require = __module.createRequire(import.meta.url)"
  , "const __dirname = __path.dirname(__url.fileURLToPath(import.meta.url))"
  , "const __filename=new URL(import.meta.url).pathname"
  ]
