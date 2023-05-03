module Spago.Command.Init
  ( run
  , InitOptions
  ) where

import Spago.Prelude

import Data.Map as Map
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Config (Dependencies(..), SetAddress(..), Config)
import Spago.Config as Config
import Spago.FS as FS
import Spago.Purs (PursEnv)

type InitOptions =
  { setVersion :: Maybe Version
  , packageName :: PackageName
  }

-- TODO run git init? Is that desirable?

run :: forall a. InitOptions -> Spago (PursEnv a) Config
run opts = do
  logInfo "Initializing a new project..."

  -- Use the specified version of the package set (if specified).
  -- Otherwise, get the latest version of the package set for the given compiler
  packageSetVersion <- Config.findPackageSet opts.setVersion

  { purs } <- ask
  logInfo $ "Initialising a new project with PureScript " <> Version.print purs.version <> " and package set " <> Version.print packageSetVersion

  -- Write config
  let config = defaultConfig opts.packageName packageSetVersion
  let configPath = "spago.yaml"
  (FS.exists configPath) >>= case _ of
    true -> logInfo $ foundExistingProject configPath
    false -> liftAff $ FS.writeYamlFile Config.configCodec configPath config

  -- If these directories (or files) exist, we skip copying "sample sources"
  -- Because you might want to just init a project with your own source files,
  -- or just migrate a psc-package project
  whenDirNotExists "src" do
    copyIfNotExists "src/Main.purs" srcMainTemplate

  whenDirNotExists "test" $ do
    FS.mkdirp "test/Test"
    copyIfNotExists "test/Test/Main.purs" testMainTemplate

  copyIfNotExists ".gitignore" gitignoreTemplate

  copyIfNotExists ".purs-repl" pursReplTemplate

  logInfo "Set up a local Spago project."
  logInfo "Try running `spago run`"
  pure config

  where
  whenDirNotExists dirPath action =
    (FS.exists dirPath) >>= case _ of
      true -> logInfo $ foundExistingDirectory dirPath
      false -> FS.mkdirp dirPath *> action

  copyIfNotExists dest srcTemplate =
    (FS.exists dest) >>= case _ of
      true -> logInfo $ foundExistingFile dest
      false -> FS.writeTextFile dest srcTemplate

-- TEMPLATES -------------------------------------------------------------------

defaultConfig :: PackageName -> Version -> Config
defaultConfig name set =
  { package: Just
      { name
      , dependencies:
          Dependencies
            ( Map.fromFoldable
                [ mkDep "effect"
                , mkDep "console"
                , mkDep "prelude"
                ]
            )
      , description: Nothing
      , build: Nothing
      , run: Nothing
      , test: Just
          { dependencies: Dependencies Map.empty
          , execArgs: Nothing
          , main: "Test.Main"
          , censorBuildWarnings: Nothing
          , censorCodes: Nothing
          , filterCodes: Nothing
          , statVerbosity: Nothing
          , showSource: Nothing
          , strict: Nothing
          , persistWarnings: Nothing
          }
      , publish: Nothing
      , bundle: Nothing
      }
  , workspace: Just
      { extra_packages: Just Map.empty
      , package_set: Just (SetFromRegistry { registry: set })
      , build_opts: Nothing
      , backend: Nothing
      }
  }
  where
  mkDep p = Tuple (unsafeFromRight $ PackageName.parse p) Nothing

srcMainTemplate ∷ String
srcMainTemplate =
  """
module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"

"""

testMainTemplate ∷ String
testMainTemplate =
  """
module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "🍕"
  log "You should add some tests."

"""

gitignoreTemplate ∷ String
gitignoreTemplate =
  """
bower_components/
node_modules/
.pulp-cache/
output/
generated-docs/
.psc-package/
.psc*
.purs*
.psa*
.spago
"""

pursReplTemplate :: String
pursReplTemplate =
  """
import Prelude
"""

-- ERROR TEXTS -----------------------------------------------------------------

foundExistingProject :: FilePath -> String
foundExistingProject path = "Found a " <> show path <> " file, skipping copy."

foundExistingDirectory :: FilePath -> String
foundExistingDirectory dir = "Found existing directory " <> show dir <> ", skipping copy of sample sources"

foundExistingFile :: FilePath -> String
foundExistingFile file = "Found existing file " <> show file <> ", not overwriting it"
