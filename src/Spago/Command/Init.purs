module Spago.Command.Init
  ( DefaultConfigOptions(..)
  , DefaultConfigPackageOptions
  , DefaultConfigWorkspaceOptions
  , InitOptions
  , defaultConfig
  , defaultConfig'
  , pursReplFile
  , run
  , srcMainTemplate
  , testMainTemplate
  ) where

import Spago.Prelude

import Data.Map as Map
import Node.Path as Path
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Config (Dependencies(..), SetAddress(..), Config)
import Spago.Config as Config
import Spago.FS as FS
import Spago.Registry (RegistryEnv)
import Spago.Registry as Registry

type InitOptions =
  -- TODO: we should allow the `--package-set` flag to alternatively pass in a URL
  { setVersion :: Maybe Version
  , packageName :: PackageName
  , useSolver :: Boolean
  }

-- TODO run git init? Is that desirable?

run :: ∀ a. InitOptions -> Spago (RegistryEnv a) Config
run opts = do
  logInfo "Initializing a new project..."

  -- Use the specified version of the package set (if specified).
  -- Otherwise, get the latest version of the package set for the given compiler
  packageSetVersion <- Registry.findPackageSet opts.setVersion

  { purs } <- ask
  logInfo $ "Found PureScript " <> Version.print purs.version <> ", will use package set " <> Version.print packageSetVersion

  -- Write config
  let
    config = defaultConfig
      { name: opts.packageName
      , withWorkspace: Just
          { setVersion: case opts.useSolver of
              true -> Nothing
              false -> Just packageSetVersion
          }
      , testModuleName: "Test.Main"
      }
  let configPath = "spago.yaml"
  (FS.exists configPath) >>= case _ of
    true -> logInfo $ foundExistingProject configPath
    false -> liftAff $ FS.writeYamlFile Config.configCodec configPath config

  -- If these directories (or files) exist, we skip copying "sample sources"
  -- Because you might want to just init a project with your own source files,
  -- or just migrate a psc-package project
  let mainModuleName = "Main"
  whenDirNotExists "src" do
    copyIfNotExists ("src" <> Path.sep <> mainModuleName <> ".purs") (srcMainTemplate mainModuleName)

  whenDirNotExists "test" $ do
    FS.mkdirp (Path.concat [ "test", "Test" ])
    copyIfNotExists (Path.concat [ "test", "Test", "Main.purs" ]) (testMainTemplate "Test.Main")

  copyIfNotExists ".gitignore" gitignoreTemplate

  copyIfNotExists pursReplFile.name pursReplFile.content

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

type TemplateConfig =
  { name :: PackageName
  , withWorkspace :: Maybe { setVersion :: Maybe Version }
  , testModuleName :: String
  }

defaultConfig :: TemplateConfig -> Config
defaultConfig { name, withWorkspace, testModuleName } = do
  let
    pkg =
      { name
      , dependencies: [ "effect", "console", "prelude" ]
      , test: Just { moduleMain: testModuleName, strict: Nothing, censorTestWarnings: Nothing, pedanticPackages: Nothing, dependencies: Nothing }
      , build: Nothing
      }
  defaultConfig' case withWorkspace of
    Nothing -> PackageOnly pkg
    Just w -> PackageAndWorkspace pkg w

type DefaultConfigPackageOptions =
  { name :: PackageName
  , dependencies :: Array String
  , test ::
      Maybe
        { moduleMain :: String
        , strict :: Maybe Boolean
        , censorTestWarnings :: Maybe Config.CensorBuildWarnings
        , pedanticPackages :: Maybe Boolean
        , dependencies :: Maybe Config.Dependencies
        }
  , build ::
      Maybe
        { strict :: Maybe Boolean
        , censorProjectWarnings :: Maybe Config.CensorBuildWarnings
        , pedanticPackages :: Maybe Boolean
        }
  }

type DefaultConfigWorkspaceOptions =
  { setVersion :: Maybe Version
  }

data DefaultConfigOptions
  = PackageOnly DefaultConfigPackageOptions
  | WorkspaceOnly DefaultConfigWorkspaceOptions
  | PackageAndWorkspace DefaultConfigPackageOptions DefaultConfigWorkspaceOptions

getDefaultConfigPackageOptions :: DefaultConfigOptions -> Maybe DefaultConfigPackageOptions
getDefaultConfigPackageOptions = case _ of
  PackageOnly pkg -> Just pkg
  PackageAndWorkspace pkg _ -> Just pkg
  WorkspaceOnly _ -> Nothing

getDefaultConfigWorkspaceOptions :: DefaultConfigOptions -> Maybe DefaultConfigWorkspaceOptions
getDefaultConfigWorkspaceOptions = case _ of
  PackageAndWorkspace _ w -> Just w
  WorkspaceOnly w -> Just w
  PackageOnly _ -> Nothing

defaultConfig' :: DefaultConfigOptions -> Config
defaultConfig' opts =
  { package: (getDefaultConfigPackageOptions opts) <#> \{ name, dependencies, test, build } ->
      { name
      , dependencies: Dependencies $ Map.fromFoldable $ map mkDep dependencies
      , description: Nothing
      , build: build <#> \{ censorProjectWarnings, strict, pedanticPackages } ->
          { censorProjectWarnings
          , strict
          , pedanticPackages
          }
      , run: Nothing
      , test: test <#> \{ moduleMain, censorTestWarnings, strict, pedanticPackages, dependencies: testDeps } ->
          { dependencies: fromMaybe (Dependencies Map.empty) testDeps
          , execArgs: Nothing
          , main: moduleMain
          , censorTestWarnings
          , strict
          , pedanticPackages
          }
      , publish: Nothing
      , bundle: Nothing
      }
  , workspace: (getDefaultConfigWorkspaceOptions opts) <#> \{ setVersion } ->
      { extraPackages: Just Map.empty
      , packageSet: setVersion # map \set -> SetFromRegistry { registry: set }
      , buildOpts: Nothing
      , backend: Nothing
      }
  }
  where
  mkDep p = Tuple (unsafeFromRight $ PackageName.parse p) Nothing

srcMainTemplate :: String -> String
srcMainTemplate moduleName = "module " <> moduleName <>
  """ where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"

"""

testMainTemplate :: String -> String
testMainTemplate moduleName = "module " <> moduleName <>
  """ where

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
output-es/
generated-docs/
.psc-package/
.psc*
.purs*
.psa*
.spago
"""

pursReplFile :: { name :: String, content :: String }
pursReplFile = { name: ".purs-repl", content: "import Prelude\n" }

-- ERROR TEXTS -----------------------------------------------------------------

foundExistingProject :: FilePath -> String
foundExistingProject path = "Found a " <> show path <> " file, skipping copy."

foundExistingDirectory :: FilePath -> String
foundExistingDirectory dir = "Found existing directory " <> show dir <> ", skipping copy of sample sources"

foundExistingFile :: FilePath -> String
foundExistingFile file = "Found existing file " <> show file <> ", not overwriting it"
