module Spago.Command.Bundle where

import Spago.Prelude

import Spago.Config (Platform)

type BundleEnv a =
  { esbuild :: FilePath
  , logOptions :: LogOptions
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
  { esbuild } <- ask
  let command = esbuild
  -- TODO: here we can select the right glob for a monorepo setup
  let minify = if opts.minify then [ "--minify" ] else []
  let
    args =
      [ "--bundle"
      , opts.entrypoint
      , "--outfile=" <> opts.outfile
      , "--platform=" <> show opts.platform
      -- See https://github.com/evanw/esbuild/issues/1921
      , "--banner:js=import __module from \'module\';import __path from \'path\';import __url from \'url\';const require = __module.createRequire(import.meta.url);"
      , "--format=esm" -- TODO: have this as input
      ] <> minify
  logDebug $ "Running esbuild: " <> show args
  void $ liftAff $ spawnFromParentWithStdin
    { command
    , args
    , input: Nothing
    , cwd: Nothing
    }
