module Spago.Command.Bundle where

import Spago.Prelude

import Data.Function.Uncurried (Fn1, runFn1)

type BundleEnv a =
  { esbuild :: FilePath
  | a
  }

data Platform = PlatformNode | PlatformBrowser

instance Show Platform where
  show = case _ of
    PlatformNode -> "node"
    PlatformBrowser -> "browser"

parsePlatform :: String -> Maybe Platform
parsePlatform = case _ of
  "node" -> Just PlatformNode
  "browser" -> Just PlatformBrowser
  _ -> Nothing

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

-- TODO: we could wire in the watch here: https://esbuild.github.io/api/#watch

-- TODO: return a promise here, so we can display the return value
foreign import bundleImpl :: Fn1 RawBundleOptions (Effect Unit)

run :: forall a. BundleOptions -> Spago (BundleEnv a) Unit
run opts = do
  -- TODO: here we can select the right glob for a monorepo setup
  let
    options =
      { minify: opts.minify
      , entryPoints: [ opts.entrypoint ]
      , format: "esm" -- TODO have this as input
      , platform: show opts.platform
      , outfile: opts.outfile
      }
  liftEffect $ runFn1 bundleImpl options
