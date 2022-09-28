module Spago.Command.Bundle where

import Spago.Prelude

import Node.Path as Path
import Spago.Config (Platform, Workspace, WorkspacePackage)

type BundleEnv a =
  { esbuild :: FilePath
  , logOptions :: LogOptions
  , workspace :: Workspace
  , selected :: WorkspacePackage
  | a
  }

type BundleOptions =
  { minify :: Boolean
  , entrypoint :: FilePath
  , outfile :: FilePath
  , platform :: Platform
  }

type RawBundleOptions =
  { minify :: Boolean
  , entryPoints :: Array FilePath
  , outfile :: FilePath
  , platform :: String
  , format :: String
  }

run :: forall a. BundleOptions -> Spago (BundleEnv a) Unit
run opts = do
  { esbuild, selected } <- ask
  let command = esbuild
  let minify = if opts.minify then [ "--minify" ] else []
  let entrypoint = Path.concat [ selected.path, opts.entrypoint ]
  let outfile = Path.concat [ selected.path, opts.outfile ]
  let
    args =
      [ "--bundle"
      , entrypoint
      , "--outfile=" <> outfile
      , "--platform=" <> show opts.platform
      -- See https://github.com/evanw/esbuild/issues/1921
      , "--banner:js=import __module from \'module\';import __path from \'path\';import __url from \'url\';const require = __module.createRequire(import.meta.url);"
      , "--format=esm" -- TODO: have this as input
      ] <> minify
  logInfo "Bundling..."
  logDebug $ "Running esbuild: " <> show args
  void $ liftAff $ spawnFromParentWithStdin
    { command
    , args
    , input: Nothing
    , cwd: Nothing
    }
  logSuccess "Bundle succeeded."
