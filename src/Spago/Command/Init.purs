module Spago.Command.Init
  ( DefaultConfigOptions(..)
  , DefaultConfigPackageOptions
  , DefaultConfigWorkspaceOptions
  , InitMode(..)
  , InitOptions
  , defaultConfig
  , defaultConfig'
  , folderToPackageName
  , pursReplFile
  , run
  , srcMainTemplate
  , testMainTemplate
  ) where

import Spago.Prelude

import Data.Array (mapMaybe)
import Data.Map as Map
import Data.String as String
import Data.String.Utils as StringUtils
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Config (Dependencies(..), SetAddress(..), Config)
import Spago.Config as Config
import Spago.FS as FS
import Spago.Log as Log
import Spago.Path as Path
import Spago.Registry (RegistryEnv)
import Spago.Registry as Registry

type InitEnv a = RegistryEnv (rootPath :: RootPath | a)

data InitMode
  = InitWorkspace { packageName :: Maybe String }
  | InitSubpackage { packageName :: String }

type InitOptions =
  -- TODO: we should allow the `--package-set` flag to alternatively pass in a URL
  { setVersion :: Maybe Version
  , mode :: InitMode
  , useSolver :: Boolean
  }

-- TODO run git init? Is that desirable?

run :: ∀ a. InitOptions -> Spago (InitEnv a) Config
run opts = do
  -- Use the specified version of the package set (if specified).
  -- Otherwise, get the latest version of the package set for the given compiler
  packageSetVersion <- Registry.findPackageSet opts.setVersion

  packageName <- getPackageName
  withWorkspace <- getWithWorkspace packageSetVersion
  projectDir <- getProjectDir packageName

  { purs } <- ask
  logInfo "Initializing a new project..."
  logInfo $ "Found PureScript " <> Version.print purs.version <> ", will use package set " <> Version.print packageSetVersion

  let
    mainModuleName = "Main"
    testModuleName = "Test.Main"
    srcDir = projectDir </> "src"
    testDir = projectDir </> "test"
    configPath = projectDir </> "spago.yaml"
    config = defaultConfig { name: packageName, withWorkspace, testModuleName }

  -- Write config
  (FS.exists configPath) >>= case _ of
    true -> logInfo $ foundExistingProject configPath
    false -> liftAff $ FS.writeYamlFile Config.configCodec configPath config

  -- If these directories (or files) exist, we skip copying "sample sources"
  -- Because you might want to just init a project with your own source files,
  -- or just migrate a psc-package project
  whenDirNotExists srcDir do
    copyIfNotExists (srcDir </> (mainModuleName <> ".purs")) (srcMainTemplate mainModuleName)

  whenDirNotExists testDir $ do
    FS.mkdirp (testDir </> "Test")
    copyIfNotExists (testDir </> "Test" </> "Main.purs") (testMainTemplate testModuleName)

  case opts.mode of
    InitWorkspace _ -> do
      copyIfNotExists (projectDir </> ".gitignore") gitignoreTemplate
      copyIfNotExists (projectDir </> pursReplFile.name) pursReplFile.content
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

  getPackageName :: Spago (InitEnv a) PackageName
  getPackageName = do
    { rootPath } <- ask
    -- When the user explicitly provides a name, validate it directly and show the actual error.
    -- When deriving from directory name, use folderToPackageName which sanitizes and gives a generic error.
    let
      explicitName = case opts.mode of
        InitWorkspace { packageName: Just n } -> Just n
        InitSubpackage { packageName: n } -> Just n
        InitWorkspace { packageName: Nothing } -> Nothing
    pname <- case explicitName of
      Just n -> case PackageName.parse (PackageName.stripPureScriptPrefix n) of
        Left err -> die
          [ toDoc "Could not figure out a name for the new package. Error:"
          , Log.break
          , Log.indent2 $ toDoc err
          ]
        Right p -> pure p
      Nothing -> do
        let candidateName = String.take 150 $ Path.basename rootPath
        case folderToPackageName candidateName of
          Nothing -> die
            [ "Could not derive a valid package name from directory " <> Path.quote rootPath <> "."
            , "Please use --name to specify a package name."
            ]
          Just p -> pure p
    logDebug [ Path.quote rootPath, PackageName.print pname ]
    logDebug [ "Got packageName and setVersion:", PackageName.print pname, unsafeStringify opts.setVersion ]
    pure pname

  getWithWorkspace :: Version -> Spago (InitEnv a) (Maybe { setVersion :: Maybe Version })
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

  getProjectDir :: PackageName -> Spago (InitEnv a) LocalPath
  getProjectDir packageName = case opts.mode of
    InitWorkspace _ ->
      ask <#> _.rootPath <#> (_ </> "")
    InitSubpackage _ -> do
      { rootPath } <- ask
      let dirPath = rootPath </> PackageName.print packageName
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

foundExistingProject :: LocalPath -> String
foundExistingProject path = "Found a " <> Path.quote path <> " file, skipping copy."

foundExistingDirectory :: LocalPath -> String
foundExistingDirectory dir = "Found existing directory " <> Path.quote dir <> ", skipping copy of sample sources"

foundExistingFile :: LocalPath -> String
foundExistingFile file = "Found existing file " <> Path.quote file <> ", not overwriting it"

-- SANITIZATION -----------------------------------------------------------------

-- | Convert a folder name to a valid package name.
-- | We try to convert as much Unicode as possible to ASCII (through NFD normalisation),
-- | and otherwise strip out and/or replace non-alpanumeric chars with dashes.
-- | After all this work that is still not enough to guarantee a successful PackageName
-- | parse, so this is still a Maybe.
folderToPackageName :: String -> Maybe PackageName
folderToPackageName input =
  input
    # String.toLower
    -- NFD normalization decomposes accented chars (é → e + combining accent)
    -- so the base ASCII letter is preserved when we filter non-ASCII later
    # StringUtils.normalize' StringUtils.NFD
    # String.toCodePointArray
    # mapMaybe sanitizeCodePoint
    # String.fromCodePointArray
    # collapseConsecutiveDashes
    # stripLeadingTrailingDashes
    # PackageName.stripPureScriptPrefix
    # PackageName.parse
    # hush
  where
  dash = String.codePointFromChar '-'

  -- Transform each codepoint:
  -- - ASCII lowercase (a-z) and digits (0-9): keep as-is
  -- - Apostrophes and quotes: remove (shouldn't create word boundaries)
  -- - Other ASCII: convert to dash (word boundaries)
  -- - Non-ASCII (combining marks from NFD, etc.): remove
  sanitizeCodePoint cp
    | isAsciiLower cp || isAsciiDigit cp = Just cp
    | isRemovable cp = Nothing
    | isAscii cp = Just dash
    | otherwise = Nothing

  isAsciiLower cp = cp >= String.codePointFromChar 'a' && cp <= String.codePointFromChar 'z'
  isAsciiDigit cp = cp >= String.codePointFromChar '0' && cp <= String.codePointFromChar '9'
  isAscii cp = cp <= String.codePointFromChar '\x7F'
  -- ASCII apostrophe and quote shouldn't create word boundaries (Tim's → tims, not tim-s)
  isRemovable cp = cp == String.codePointFromChar '\'' || cp == String.codePointFromChar '"'

  -- Collapse consecutive dashes into one
  collapseConsecutiveDashes str =
    case String.indexOf (String.Pattern "--") str of
      Nothing -> str
      Just _ -> collapseConsecutiveDashes $ String.replaceAll (String.Pattern "--") (String.Replacement "-") str

  -- Remove all leading and trailing dashes
  stripLeadingTrailingDashes str =
    case String.stripPrefix (String.Pattern "-") str of
      Just stripped -> stripLeadingTrailingDashes stripped
      Nothing -> case String.stripSuffix (String.Pattern "-") str of
        Just stripped -> stripLeadingTrailingDashes stripped
        Nothing -> str
