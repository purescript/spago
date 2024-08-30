module Spago.Command.Init
  ( DefaultConfigOptions(..)
  , DefaultConfigPackageOptions
  , DefaultConfigWorkspaceOptions
  , InitMode(..)
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
import Data.String as String
import Node.Path as Path
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Config (Dependencies(..), SetAddress(..), Config)
import Spago.Config as Config
import Spago.FS as FS
import Spago.Log as Log
import Spago.Paths as Paths
import Spago.Registry (RegistryEnv)
import Spago.Registry as Registry

data InitMode = InitWorkspace (Maybe String) | InitSubpackage String

type InitOptions =
  -- TODO: we should allow the `--package-set` flag to alternatively pass in a URL
  { setVersion :: Maybe Version
  , mode :: InitMode
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

  packageName <- getPackageName
  withWorkspace <- getWithWorkspace packageSetVersion
  projectDir <- getProjectDir packageName

  let
    mainModuleName = "Main"
    testModuleName = "Test.Main"
    srcDir = Path.concat [ projectDir, "src" ]
    testDir = Path.concat [ projectDir, "test" ]
    configPath = Path.concat [ projectDir, "spago.yaml" ]
    config = defaultConfig { name: packageName, withWorkspace, testModuleName }

  -- Write config
  (FS.exists configPath) >>= case _ of
    true -> logInfo $ foundExistingProject configPath
    false -> liftAff $ FS.writeYamlFile Config.configCodec configPath config

  -- If these directories (or files) exist, we skip copying "sample sources"
  -- Because you might want to just init a project with your own source files,
  -- or just migrate a psc-package project
  whenDirNotExists srcDir do
    copyIfNotExists (Path.concat [ srcDir, mainModuleName <> ".purs" ]) (srcMainTemplate mainModuleName)

  whenDirNotExists testDir $ do
    FS.mkdirp (Path.concat [ testDir, "Test" ])
    copyIfNotExists (Path.concat [ testDir, "Test", "Main.purs" ]) (testMainTemplate testModuleName)

  case opts.mode of
    InitWorkspace _ -> do
      copyIfNotExists ".gitignore" gitignoreTemplate
      copyIfNotExists pursReplFile.name pursReplFile.content
    InitSubpackage _ ->
      pure unit

  logInfo "Set up a new Spago project."
  case opts.mode of
    InitWorkspace _ -> logInfo "Try running `spago run`"
    InitSubpackage _ -> logInfo $ "Try running `spago run -p " <> PackageName.print packageName <> "`"

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

  getPackageName = do
    let candidateName = case opts.mode of
          InitWorkspace Nothing -> String.take 150 $ Path.basename Paths.cwd
          InitWorkspace (Just n) -> n
          InitSubpackage n -> n
    logDebug [ show Paths.cwd, show candidateName ]
    pname <- case PackageName.parse (PackageName.stripPureScriptPrefix candidateName) of
      Left err -> die
        [ toDoc "Could not figure out a name for the new package. Error:"
        , Log.break
        , Log.indent2 $ toDoc err
        ]
      Right p -> pure p
    logDebug [ "Got packageName and setVersion:", PackageName.print pname, unsafeStringify opts.setVersion ]
    pure pname

  getWithWorkspace setVersion = case opts.mode of
    InitWorkspace _ ->
      pure $ Just
        { setVersion: case opts.useSolver of
            true -> Nothing
            false -> Just setVersion
        }
    InitSubpackage _ -> do
      when (isJust opts.setVersion || opts.useSolver) do
        logWarn "The --package-set and --use-solver flags are ignored when initializing a subpackage"
      pure Nothing

  getProjectDir packageName = case opts.mode of
    InitWorkspace _ ->
      pure "."
    InitSubpackage _ -> do
      let dirPath = PackageName.print packageName
      unlessM (FS.exists dirPath) $ FS.mkdirp dirPath
      pure dirPath


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
foundExistingProject path = "Found a " <> path <> " file, skipping copy."

foundExistingDirectory :: FilePath -> String
foundExistingDirectory dir = "Found existing directory " <> dir <> ", skipping copy of sample sources"

foundExistingFile :: FilePath -> String
foundExistingFile file = "Found existing file " <> file <> ", not overwriting it"
