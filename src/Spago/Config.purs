module Spago.Config
  ( BuildType(..)
  , Package(..)
  , PackageMap
  , PackageSet(..)
  , WithTestGlobs(..)
  , Workspace
  , WorkspaceBuildOptions
  , WorkspacePackage
  , addOwner
  , addPackagesToConfig
  , addPublishLocationToConfig
  , addRangesToConfig
  , configDocMissingErrorMessage
  , fileSystemCharEscape
  , getLocalPackageLocation
  , getTopologicallySortedWorkspacePackages
  , getWorkspacePackages
  , isRootPackage
  , module Core
  , readConfig
  , discoverWorkspace
  , removePackagesFromConfig
  , setPackageSetVersionInConfig
  , sourceGlob
  , workspacePackageToLockfilePackage
  ) where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.State as State
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode as Unicode
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Enum as Enum
import Data.Foldable (traverse_)
import Data.Graph as Graph
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Map as Map
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String (CodePoint)
import Data.String as String
import Dodo as Log
import Effect.Aff as Aff
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Foreign.Object as Foreign
import JSON (JSON)
import Node.Path as Node.Path
import Registry.Internal.Codec as Internal.Codec
import Registry.Owner (Owner(..))
import Registry.PackageName as PackageName
import Registry.PackageSet as Registry.PackageSet
import Registry.Range as Range
import Registry.Version as Version
import Spago.Core.Config as Core
import Spago.FS as FS
import Spago.Glob as Glob
import Spago.Json as Json
import Spago.Lock (Lockfile, PackageSetInfo)
import Spago.Lock as Lock
import Spago.Path as Path
import Spago.Paths as Paths
import Spago.Registry as Registry
import Spago.Yaml as Yaml

type Workspace =
  { selected :: Maybe WorkspacePackage
  , packageSet :: PackageSet
  , compatibleCompiler :: Range
  , backend :: Maybe Core.BackendConfig
  , buildOptions :: WorkspaceBuildOptions
  , doc :: Maybe (YamlDoc Core.Config)
  , workspaceConfig :: Core.WorkspaceConfig
  , rootPackage :: Maybe Core.PackageConfig
  }

type WorkspaceBuildOptions =
  { output :: Maybe LocalPath
  , censorLibWarnings :: Maybe Core.CensorBuildWarnings
  , statVerbosity :: Maybe Core.StatVerbosity
  }

fromExtraPackage :: Core.ExtraPackage -> Package
fromExtraPackage = case _ of
  Core.ExtraLocalPackage lp -> LocalPackage lp
  Core.ExtraRemotePackage rp -> fromRemotePackage rp

fromRemotePackage :: Core.RemotePackage -> Package
fromRemotePackage = case _ of
  Core.RemoteGitPackage p -> GitPackage p
  Core.RemoteRegistryVersion v -> RegistryVersion v
  Core.RemoteLegacyPackage e -> GitPackage
    { git: e.repo
    , ref: e.version
    , subdir: Nothing
    , dependencies: Just $ Core.Dependencies $ Map.fromFoldable $ map (\p -> Tuple p Nothing) e.dependencies
    }

-- | The format of a legacy packages.json package set file
newtype LegacyPackageSet = LegacyPackageSet (Map PackageName Core.LegacyPackageSetEntry)

derive instance Newtype LegacyPackageSet _
derive newtype instance Eq LegacyPackageSet

legacyPackageSetCodec :: CJ.Codec LegacyPackageSet
legacyPackageSetCodec =
  Profunctor.wrapIso LegacyPackageSet
    $ Internal.Codec.packageMap Core.legacyPackageSetEntryCodec

newtype RemotePackageSet = RemotePackageSet
  { compiler :: Version
  , packages :: Map PackageName Core.RemotePackage
  , version :: Version
  }

remotePackageSetCodec :: CJ.Codec RemotePackageSet
remotePackageSetCodec = Profunctor.wrapIso RemotePackageSet $ CJ.named "PackageSet" $ CJ.Record.object
  { version: Version.codec
  , compiler: Version.codec
  , packages: Internal.Codec.packageMap Core.remotePackageCodec
  }

derive instance Newtype RemotePackageSet _
derive newtype instance Eq RemotePackageSet

type PackageMap = Map PackageName Package

data BuildType
  = RegistrySolverBuild PackageMap
  | PackageSetBuild PackageSetInfo PackageMap

type PackageSet =
  { buildType :: BuildType
  , lockfile :: Either String Lockfile
  }

type WorkspacePackage =
  { path :: LocalPath
  , package :: Core.PackageConfig
  , doc :: Maybe (YamlDoc Core.Config)
  , hasTests :: Boolean
  }

data Package
  = RegistryVersion Version
  | GitPackage Core.GitPackage
  | LocalPackage Core.LocalPackage
  | WorkspacePackage WorkspacePackage

type ReadWorkspaceConfigResult =
  { config :: ReadConfigResult
  , hasTests :: Boolean
  , configPath :: LocalPath
  , packagePath :: LocalPath
  }

type ReadWorkspaceOptions =
  { maybeSelectedPackage :: Maybe PackageName
  , pureBuild :: Boolean
  , migrateConfig :: Boolean
  }

isRootPackage :: WorkspacePackage -> Boolean
isRootPackage p = Path.localPart p.path == ""

spagoYaml = "spago.yaml" :: String

discoverWorkspace :: ∀ a. ReadWorkspaceOptions -> GlobalPath -> Spago (Registry.RegistryEnv a) { workspace :: Workspace, rootPath :: RootPath }
discoverWorkspace options cwd = do
  logInfo "Reading Spago workspace configuration..."
  logDebug $ "Discovering nearest workspace " <> spagoYaml <> " starting at " <> Path.quote cwd

  { workspace, rootPath } /\ { loadedPackages, closestPackage } <-
    State.runStateT (walkDirectoriesUpFrom cwd)
      { loadedPackages: Map.empty, otherWorkspaceRoots: [], misnamedConfigs: [], closestPackage: Nothing }

  migrateConfigsWhereNeeded rootPath loadedPackages

  packagesByName <-
    Map.fromFoldable <$>
      for (Map.toUnfoldable loadedPackages :: Array _) \(path /\ { package, config }) -> do
        hasTests <- FS.exists (path </> "test")
        let
          wsp :: WorkspacePackage
          wsp = { package, path: path `Path.relativeTo` rootPath, doc: Just config.doc, hasTests }
        pure (package.name /\ wsp)

  selected <-
    determineSelectedPackage
      { explicitlySelected: options.maybeSelectedPackage
      , inferredFromCwd: closestPackage
      , rootPackage: workspace.rootPackage <#> _.name
      , loadedPackages: packagesByName
      }

  lockfile <-
    loadLockfile { pureBuild: options.pureBuild, workspaceConfig: workspace.config, loadedPackages: packagesByName, rootPath }

  { packageSet, compiler } <-
    loadPackageSet { workspaceConfig: workspace.config, loadedPackages: packagesByName, rootPath, lockfile }

  pure
    { rootPath
    , workspace:
        { selected
        , packageSet
        , compatibleCompiler: compiler
        , backend: workspace.config.backend
        , buildOptions:
            { output: workspace.config.buildOpts >>= _.output <#> \o -> withForwardSlashes $ rootPath </> o
            , censorLibWarnings: _.censorLibraryWarnings =<< workspace.config.buildOpts
            , statVerbosity: _.statVerbosity =<< workspace.config.buildOpts
            }
        , doc: Just workspace.doc
        , workspaceConfig: workspace.config
        , rootPackage: workspace.rootPackage
        }
    }
  where
  readConfig' = State.lift <<< readConfig

  walkDirectoriesUpFrom dir = do
    maybeConfig <- tryReadConfigAt configFile

    for_ maybeConfig \config ->
      for_ config.yaml.package \package ->
        -- If there is a package in this directory, remember it
        State.modify_ \s -> s
          { loadedPackages = Map.insert dir { package, config } s.loadedPackages
          , closestPackage = s.closestPackage <|> Just package.name
          }

    whenM (FS.exists $ dir </> "spago.yml") $
      State.modify_ \s -> s { misnamedConfigs = Array.cons dir s.misnamedConfigs }

    case maybeConfig of
      Just { doc, yaml: { workspace: Just workspace, package } } -> do
        -- Finally, found the "workspace" config!
        rootPath <- Path.mkRoot dir
        loadSubprojectConfigs rootPath
        pure { workspace: { config: workspace, doc, rootPackage: package }, rootPath }
      _ -> do
        -- No workspace in this directory => recur to parent directory (unless it's already root)
        when (parentDir == dir) $
          dieForLackOfSpagoYaml
        walkDirectoriesUpFrom parentDir

    where
    configFile = dir </> spagoYaml
    parentDir = Path.dirname dir

  loadSubprojectConfigs rootPath = do
    candidates <- liftAff $ Glob.gitignoringGlob
      { root: rootPath
      , includePatterns: [ "**/" <> spagoYaml ]
      , ignorePatterns: [ "**/node_modules/**", "**/.spago/**" ]
      }

    -- Traversing directories (not files) and doing it in sorted order ensures
    -- that parent directories come before their subdirectories. That way we
    -- can remember workspaces that we find along the way and avoid trying to
    -- load their subprojects that come later.
    candidates <#> Path.toGlobal <#> Path.dirname # Array.sort # traverse_ \dir -> do
      st <- State.get
      let
        configFile = dir </> spagoYaml
        alreadyLoaded = st.loadedPackages # Map.member configFile
        anotherParentWorkspace = st.otherWorkspaceRoots # Array.find (_ `Path.isPrefixOf` dir)
      case alreadyLoaded, anotherParentWorkspace of
        true, _ ->
          pure unit
        _, Just ws -> do
          logDebug $ "Not trying to load " <> Path.quote configFile <> " because it belongs to a different workspace at " <> Path.quote ws
          pure unit
        false, Nothing ->
          readConfig' configFile >>= case _ of
            Left _ ->
              logWarn $ "Failed to read config at " <> Path.quote configFile
            Right { yaml: { workspace: Just _ } } ->
              State.modify_ \s -> s { otherWorkspaceRoots = Array.cons dir s.otherWorkspaceRoots }
            Right config@{ yaml: { package: Just package } } -> do
              logDebug $ "Loaded a subproject config at " <> Path.quote configFile
              State.modify_ \s -> s { loadedPackages = Map.insert dir { package, config } s.loadedPackages }
            Right _ -> do
              logWarn $ "Neither workspace nor package found in " <> Path.quote configFile

  tryReadConfigAt path = do
    exists <- FS.exists path
    if exists then
      Just <$> do
        logDebug $ "Loading spago.yaml at " <> Path.quote path
        readConfig' path >>= rightOrDieWith \errLines ->
          [ toDoc $ "Couldn't parse Spago config file at: " <> Path.quote path
          , indent $ toDoc errLines
          , Log.break
          , toDoc "The configuration file help can be found here https://github.com/purescript/spago#the-configuration-file"
          ]
    else
      pure Nothing

  migrateConfigsWhereNeeded rootPath loadedConfigs = do
    forWithIndex_ loadedConfigs \path' { config } -> do
      let path = (path' </> spagoYaml) `Path.relativeTo` rootPath
      case options.migrateConfig, config.wasMigrated of
        true, true -> do
          logInfo $ "Migrating your " <> Path.quote path <> " to the latest version..."
          liftAff $ FS.writeYamlDocFile path config.doc
        false, true ->
          logWarn $ "Your " <> Path.quote path <> " is using an outdated format. Run Spago with the --migrate flag to update it to the latest version."
        _, false ->
          pure unit

  dieForLackOfSpagoYaml = do
    root <- Path.mkRoot cwd
    misnamedConfigs <- State.gets _.misnamedConfigs
    let
      misnamedConfigsList =
        case misnamedConfigs <#> \c -> Path.quote $ (c </> "spago.yml") `Path.relativeTo` root of
          [] -> []
          [ one ] -> [ toDoc $ "Instead found " <> one ]
          many -> [ toDoc "Instead found these:", indent $ toDoc many ]
    die
      [ toDoc $ "No " <> spagoYaml <> " found in the current directory or any of its parents."
      , if Array.null misnamedConfigsList then
          toDoc ""
        else
          toDoc
            [ Log.break
            , indent $ toDoc $ misnamedConfigsList
            , indent $ toDoc $ "Note that Spago config files should be named " <> spagoYaml <> ", not spago.yml."
            , Log.break
            , toDoc "The configuration file help can be found here https://github.com/purescript/spago#the-configuration-file"
            ]
      ]

determineSelectedPackage
  :: ∀ a
   . { explicitlySelected :: Maybe PackageName
     , inferredFromCwd :: Maybe PackageName
     , rootPackage :: Maybe PackageName
     , loadedPackages :: Map PackageName WorkspacePackage
     }
  -> Spago (Registry.RegistryEnv a) (Maybe WorkspacePackage)
determineSelectedPackage { explicitlySelected, inferredFromCwd, rootPackage, loadedPackages } = do
  let
    inferredFromCwd' =
      -- Do not auto-select the root package if Spago is being run from the root directory.
      if inferredFromCwd == rootPackage then Nothing else inferredFromCwd

    selectedName =
      explicitlySelected <|> inferredFromCwd'

  -- Select the package for spago to handle during the rest of the execution
  maybeSelected <- case selectedName of
    Nothing -> case Array.uncons (Map.toUnfoldable loadedPackages) of
      Nothing -> die "No valid packages found in the current project, halting."
      -- If there's only one package and it's not in the root we still select that
      Just { head: (Tuple packageName package), tail: [] } -> do
        logDebug $ "Selecting package " <> PackageName.print packageName <> " from " <> Path.quote package.path
        pure (Just package)
      -- If no package has been selected and we have many packages, then we build all of them but select none
      _ -> pure Nothing
    Just name -> case Map.lookup name loadedPackages of
      Nothing -> die
        $ [ toDoc $ "Selected package " <> PackageName.print name <> " was not found in the local packages." ]
        <> case (Array.fromFoldable $ Map.keys loadedPackages) of
          [] ->
            [ toDoc "No available packages." ]
          pkgs ->
            case typoSuggestions PackageName.print name pkgs of
              [] -> [ toDoc "All available packages:", indent (toDoc pkgs) ]
              suggestions -> [ toDoc "Did you mean:", indent (toDoc suggestions) ]
      Just p ->
        pure (Just p)

  -- Figure out if we are selecting a single package or not
  case maybeSelected of
    Just selected -> do
      logSuccess $ "Selecting package to build: " <> PackageName.print selected.package.name
      logDebug $ "Package path: " <> Path.quote selected.path
    Nothing -> do
      logSuccess
        [ toDoc $ "Selecting " <> show (Map.size loadedPackages) <> " packages to build:"
        , indent2 (toDoc (Set.toUnfoldable $ Map.keys loadedPackages :: Array PackageName))
        ]

  pure maybeSelected

loadLockfile
  :: ∀ a
   . { pureBuild :: Boolean
     , workspaceConfig :: Core.WorkspaceConfig
     , loadedPackages :: Map PackageName WorkspacePackage
     , rootPath :: RootPath
     }
  -> Spago (Registry.RegistryEnv a) (Either String Lockfile)
loadLockfile { pureBuild, workspaceConfig, loadedPackages, rootPath } = do
  logDebug "Parsing the lockfile..."
  let lockFilePath = rootPath </> "spago.lock"
  FS.exists lockFilePath >>= case _ of
    false -> pure (Left "No lockfile found")
    true -> liftAff (FS.readJsonFile Lock.lockfileCodec lockFilePath) >>= case _ of
      Left error -> do
        logWarn
          [ "Your project contains a spago.lock file, but it cannot be decoded. Spago will generate a new one."
          , "Error was: " <> error
          ]
        pure (Left "Could not decode lockfile")
      -- Here we figure out if the lockfile is still up to date by having a quick look at the configurations:
      -- if they changed since the last write, then we need to regenerate the lockfile
      -- Unless! the user is passing the --pure flag, in which case we just use the lockfile
      Right contents -> do
        logDebug "Parsed the lockfile"
        case pureBuild, shouldComputeNewLockfile { workspace: workspaceConfig, workspacePackages: loadedPackages } contents.workspace of
          true, _ -> do
            logDebug "Using lockfile because of --pure flag"
            pure (Right contents)
          false, lockfileIsOutOfDate@{ result: true } -> do
            logDebug $ "Reason for recomputing the lockfile: " <> show lockfileIsOutOfDate
            pure $ Left $ "Lockfile is out of date (reason: " <> lockfileIsOutOfDate.reasons <> ")"
          false, { result: false } -> do
            logDebug "Lockfile is up to date, using it"
            pure (Right contents)

loadPackageSet
  :: ∀ a
   . { workspaceConfig :: Core.WorkspaceConfig
     , loadedPackages :: Map PackageName WorkspacePackage
     , rootPath :: RootPath
     , lockfile :: Either String Lockfile
     }
  -> Spago (Registry.RegistryEnv a) { packageSet :: PackageSet, compiler :: Range }
loadPackageSet { lockfile, workspaceConfig, loadedPackages, rootPath } = do
  { offline } <- ask
  packageSetInfo <- case lockfile, workspaceConfig.packageSet of
    _, Nothing -> do
      logDebug "Did not find a package set in your config, using Registry solver"
      pure Nothing

    -- If there's a lockfile we don't attempt to fetch the package set from the registry
    -- repo nor from the internet, since we already have the whole set right there
    Right lf, _ -> do
      logDebug "Found the lockfile, using the package set from there"
      pure lf.workspace.package_set

    Left reason, Just address@(Core.SetFromRegistry { registry: v }) -> do
      logDebug reason
      logDebug "Reading the package set from the Registry repo..."
      (Registry.PackageSet.PackageSet registryPackageSet) <- Registry.readPackageSet v
      logDebug "Read the package set from the Registry repo"
      pure $ Just
        { content: map Core.RemoteRegistryVersion registryPackageSet.packages
        , address
        , compiler: Range.caret registryPackageSet.compiler
        }

    Left reason, Just address@(Core.SetFromPath { path }) -> do
      logDebug reason
      logDebug $ "Reading the package set from local path: " <> path
      liftAff (FS.readJsonFile remotePackageSetCodec (rootPath </> path)) >>= case _ of
        Left err -> die $ "Couldn't read the package set: " <> err
        Right (RemotePackageSet localPackageSet) -> do
          logInfo "Read the package set from local path"
          pure $ Just
            { content: localPackageSet.packages
            , address
            , compiler: Range.caret localPackageSet.compiler
            }

    Left reason, Just address@(Core.SetFromUrl { url: rawUrl }) -> do
      result <- case offline of
        Offline -> die "You are offline, but the package set is not cached locally. Please connect to the internet and try again."
        _ -> do
          logDebug reason
          logDebug $ "Reading the package set from URL: " <> rawUrl
          url <- case parseUrl rawUrl of
            Left err -> die $ "Could not parse URL for the package set, error: " <> show err
            Right u -> pure u.href
          response <- liftAff $ Http.request (Http.defaultRequest { method = Left Method.GET, responseFormat = Response.string, url = url })
          case response of
            Left err -> die $ "Couldn't fetch package set:\n  " <> Http.printError err
            Right { status, body } | status /= StatusCode 200 -> do
              die $ "Couldn't fetch package set, status was not ok " <> show status <> ", got answer:\n  " <> body
            Right r@{ body } -> do
              logDebug $ "Fetching package set - got status: " <> show r.status
              case parseJson remotePackageSetCodec body of
                Right (RemotePackageSet set) -> do
                  logDebug "Read a new-format package set from URL"
                  pure { compiler: set.compiler, remotePackageSet: set.packages }
                Left err -> do
                  logDebug [ "Couldn't parse remote package set in modern format, error:", "  " <> show err, "Trying with the legacy format..." ]
                  case parseJson legacyPackageSetCodec body of
                    Left err' -> die $ "Couldn't parse remote package set, error: " <> show err'
                    Right (LegacyPackageSet set) -> do
                      logDebug "Read legacy package set from URL"
                      version <- case Map.lookup (unsafeFromRight (PackageName.parse "metadata")) set of
                        Just { version } -> pure (unsafeFromRight (parseLenientVersion version))
                        Nothing -> die $ "Couldn't find 'metadata' package in legacy package set."
                      pure { compiler: version, remotePackageSet: map Core.RemoteLegacyPackage set }
      pure $ Just
        { content: result.remotePackageSet
        , compiler: Range.caret result.compiler
        , address
        }

  -- Mix in the package set (a) the workspace packages, and (b) the extra_packages
  -- Note:
  -- 1. we error out if any workspace package collides with any package in the database,
  --    because it's just a confusing situation.
  -- 2. then, if there are duplicate packages we pick the "most local ones first",
  --    i.e. first workspace, then extra, then registry packages.
  -- This is to (1) easily allow overriding packages, (2) easily allow "private registries"
  -- and (3) prevent the security hole where people can register new names and take precedence in your build.
  let
    extraPackages = map fromExtraPackage (fromMaybe Map.empty workspaceConfig.extraPackages)
    localPackagesOverlap = Set.intersection (Map.keys loadedPackages) (Map.keys extraPackages)
    localPackages = Map.union (map WorkspacePackage loadedPackages) extraPackages
    buildType = case packageSetInfo of
      Nothing -> RegistrySolverBuild localPackages
      Just info -> PackageSetBuild info $ Map.union localPackages (map fromRemotePackage info.content)

  -- Note again: we only try to prevent collisions between workspace packages and local overrides.
  -- We otherwise want local packages to override _remote_ ones, e.g. in the case where you are
  -- developing a library that is in the package set / registry.
  unless (Set.isEmpty localPackagesOverlap) do
    die
      $
        [ toDoc "Some packages in your local tree overlap with ones you have declared in your workspace configuration."
        , toDoc "To resolve this error, rename these packages declared in `extra_packages` to a different name:"
        ]
      <> map (\p -> indent $ toDoc $ "- " <> PackageName.print p) (Array.fromFoldable localPackagesOverlap)

  pure
    { packageSet: { buildType, lockfile }
    , compiler: packageSetInfo <#> _.compiler # fromMaybe Core.widestRange
    }

workspacePackageToLockfilePackage :: WorkspacePackage -> Tuple PackageName Lock.WorkspaceLockPackage
workspacePackageToLockfilePackage { path, package } = Tuple package.name
  { path: case Path.localPart (withForwardSlashes path) of
      "" -> "./"
      p -> p
  , core: { dependencies: package.dependencies, build_plan: mempty }
  , test: { dependencies: foldMap _.dependencies package.test, build_plan: mempty }
  }

type LockfileRecomputeResult =
  { workspacesDontMatch :: Boolean
  , extraPackagesDontMatch :: Boolean
  , packageSetAddressIsDifferent :: Boolean
  , packageSetIsLocal :: Boolean
  , result :: Boolean
  , reasons :: String
  }

shouldComputeNewLockfile :: { workspace :: Core.WorkspaceConfig, workspacePackages :: Map PackageName WorkspacePackage } -> Lock.WorkspaceLock -> LockfileRecomputeResult
shouldComputeNewLockfile { workspace, workspacePackages } workspaceLock =
  { workspacesDontMatch
  , extraPackagesDontMatch
  , packageSetAddressIsDifferent
  , packageSetIsLocal
  , result: workspacesDontMatch || extraPackagesDontMatch || packageSetAddressIsDifferent || packageSetIsLocal
  , reasons: String.joinWith ", " $ Array.mapMaybe identity
      [ explainReason workspacesDontMatch "workspace packages changed"
      , explainReason extraPackagesDontMatch "extraPackages changed"
      , explainReason packageSetAddressIsDifferent "package set address changed"
      , explainReason packageSetIsLocal "package set is local"
      ]
  }
  where
  eraseBuildPlan = _ { core { build_plan = mempty }, test { build_plan = mempty } }
  -- surely this already exists
  explainReason flag reason = if flag then Just reason else Nothing

  -- Conditions for recomputing the lockfile:
  -- 1. the workspace packages should exactly match, except for the needed_by field, which is filled in during build plan construction
  workspacesDontMatch = (workspacePackageToLockfilePackage >>> snd <$> workspacePackages) /= (eraseBuildPlan <$> workspaceLock.packages)
  -- 2. the extra packages should exactly match
  extraPackagesDontMatch = fromMaybe Map.empty workspace.extraPackages /= workspaceLock.extra_packages
  -- 3. the package set address needs to match - we have no way to match the package set contents at this point, so we let it be
  packageSetAddressIsDifferent = workspace.packageSet /= map _.address workspaceLock.package_set
  -- 4. the package set is not a local file - if it is then we always recompute the lockfile because we have no way to check if it's changed
  packageSetIsLocal = case workspace.packageSet of
    Just (Core.SetFromPath _) -> true
    _ -> false

getLocalPackageLocation :: RootPath -> PackageName -> Package -> LocalPath
getLocalPackageLocation root name = case _ of
  RegistryVersion v -> root </> Paths.localCachePackagesPath </> (PackageName.print name <> "-" <> Version.print v)
  GitPackage p -> root </> Paths.localCachePackagesPath </> PackageName.print name </> fileSystemCharEscape p.ref
  LocalPackage p -> root </> p.path
  WorkspacePackage { path } -> path

-- This function must be injective and must always produce valid directory
-- names, which means that problematic characters like '/' or ':' will be escaped
-- using a scheme similar to URL-encoding. Note in particular that the function
-- must be injective in a case-insensitive manner if we want this to work
-- reliably on case-insensitive filesystems, in the sense that two different
-- inputs must map to two different outputs _and_ those outputs must differ by
-- more than just casing.
--
-- The characters which are most commonly used in version and branch names are
-- those which we allow through as they are (without escaping).
fileSystemCharEscape :: String -> String
fileSystemCharEscape = String.toCodePointArray >>> map escapeCodePoint >>> Array.fold
  where
  commonlyUsedChars = map String.codePointFromChar [ '.', ',', '-', '_', '+' ]
  ignoreEscape = Unicode.isLower || Unicode.isDecDigit || flip Array.elem commonlyUsedChars

  escapeCodePoint :: CodePoint -> String
  escapeCodePoint cp
    | ignoreEscape cp = String.singleton cp
    | otherwise = append "%" $ Int.toStringAs Int.hexadecimal $ Enum.fromEnum cp

data WithTestGlobs
  = WithTestGlobs
  | NoTestGlobs
  | OnlyTestGlobs

sourceGlob :: RootPath -> WithTestGlobs -> PackageName -> Package -> Array LocalPath
sourceGlob root withTestGlobs name package = map (\p -> getLocalPackageLocation root name package </> p)
  case package of
    WorkspacePackage { hasTests } ->
      case hasTests, withTestGlobs of
        false, OnlyTestGlobs -> []
        false, _ -> [ srcGlob ]
        true, OnlyTestGlobs -> [ testGlob ]
        true, NoTestGlobs -> [ srcGlob ]
        true, WithTestGlobs -> [ srcGlob, testGlob ]
    GitPackage { subdir: Just s } -> [ Node.Path.concat [ s, srcGlob ] ]
    _ -> [ srcGlob ]

srcGlob :: String
srcGlob = "src/**/*.purs"

testGlob :: String
testGlob = "test/**/*.purs"

-- We can afford an unsafe here, if it's empty we have bigger problems
getWorkspacePackages :: PackageSet -> NonEmptyArray WorkspacePackage
getWorkspacePackages = unsafeFromJust <<< NEA.fromFoldable <<< Array.mapMaybe extractWorkspacePackage <<< Map.toUnfoldable <<< extractSet <<< _.buildType
  where
  extractSet = case _ of
    PackageSetBuild _info m -> m
    RegistrySolverBuild m -> m
  extractWorkspacePackage = case _ of
    Tuple _ (WorkspacePackage p) -> Just p
    _ -> Nothing

getTopologicallySortedWorkspacePackages :: PackageSet -> Array WorkspacePackage
getTopologicallySortedWorkspacePackages packageSet = do
  let
    packageMap = Map.fromFoldable $ map (\p -> Tuple p.package.name p) $ getWorkspacePackages packageSet
    dependenciesAsList p = Set.toUnfoldable $ Map.keys $ unwrap p.package.dependencies
    topSortPkgs =
      Array.reverse
        $ Array.fromFoldable
        $ Graph.topologicalSort
        $ Graph.fromMap
        $ map (\p -> Tuple p $ dependenciesAsList p) packageMap
  Array.mapMaybe (flip Map.lookup packageMap) topSortPkgs

--------------------------------------------------------------------------------
-- Yaml handling

type ReadConfigResult = { doc :: YamlDoc Core.Config, yaml :: Core.Config, wasMigrated :: Boolean }

readConfig :: ∀ a path. Path.IsPath path => path -> Spago (SpagoBaseEnv a) (Either (Array String) ReadConfigResult)
readConfig path = do
  logDebug $ "Reading config from " <> Path.quote path
  try (FS.readTextFile path) >>= case _ of
    Left err -> do
      logDebug $ "Could not read file " <> Path.quote path <> ", error: " <> Aff.message err
      let altConfigName = Path.replaceExtension (String.Pattern ".yaml") (String.Replacement ".yml") path
      yml <- map join $ for altConfigName \yml -> do
        hasYml <- FS.exists yml
        pure $
          if hasYml then
            Just yml
          else
            Nothing
      pure $ Left $ case yml of
        Just y ->
          [ "Did not find " <> Path.quote path <> ". Spago's configuration files must end with `.yaml`, not `.yml`."
          , "Try renaming " <> Path.basename y <> " to " <> Path.basename path <> " or run `spago init` to initialize a new project."
          ]
        Nothing | Path.basename path == spagoYaml ->
          [ "Did not find " <> Path.quote path <> ". Run `spago init` to initialize a new project." ]
        Nothing ->
          [ "Did not find " <> Path.quote path <> "." ]
    Right yamlString -> do
      case lmap (\err -> CJ.DecodeError.basic ("YAML: " <> err)) (Yaml.parser yamlString) of
        Left err -> pure $ Left [ CJ.DecodeError.print err ]
        Right doc -> do
          -- At this point we are sure that we have a valid Yaml document in `doc`,
          -- and it's just a matter of decoding it into our `Config` type.
          -- We don't have a "strict" decoder, in the sense that there are optional
          -- and extra keys, no particular canary to check for, and we don't detect
          -- superfluous keys in documents - this is a problem, because if we had all
          -- of that then we'd just try to parse with the new codec, then fall back to
          -- the old one if that failed, etc.
          -- Instead we can only apply the transforms to the doc, and then try to decode it as is.
          -- We try to decode with the old decoder first.
          -- TODO: revisit this once we have #1165, to parse more strictly
          let maybeMigratedDoc = Nullable.toMaybe (migrateV1ConfigImpl doc)
          pure $ bimap
            Json.printConfigError
            (\yaml -> { doc, yaml, wasMigrated: isJust maybeMigratedDoc })
            (CJ.decode Core.configCodec (Yaml.toJson $ fromMaybe doc maybeMigratedDoc))

setPackageSetVersionInConfig :: forall m. MonadAff m => MonadEffect m => RootPath -> YamlDoc Core.Config -> Version -> m Unit
setPackageSetVersionInConfig root doc version = do
  liftEffect $ runEffectFn2 setPackageSetVersionInConfigImpl doc (Version.print version)
  liftAff $ FS.writeYamlDocFile (root </> spagoYaml) doc

addPackagesToConfig :: forall m path. Path.IsPath path => MonadAff m => path -> YamlDoc Core.Config -> Boolean -> Array PackageName -> m Unit
addPackagesToConfig configPath doc isTest pkgs = do
  liftEffect $ runEffectFn3 addPackagesToConfigImpl doc isTest (map PackageName.print pkgs)
  liftAff $ FS.writeYamlDocFile configPath doc

removePackagesFromConfig :: YamlDoc Core.Config -> Boolean -> NonEmptySet PackageName -> Effect Unit
removePackagesFromConfig doc isTest pkgs = runEffectFn3 removePackagesFromConfigImpl doc isTest (flip NonEmptySet.member pkgs)

addRangesToConfig :: YamlDoc Core.Config -> Map PackageName Range -> Effect Unit
addRangesToConfig doc = runEffectFn2 addRangesToConfigImpl doc
  <<< Foreign.fromFoldable
  <<< map (\(Tuple name range) -> Tuple (PackageName.print name) (Core.printSpagoRange range))
  <<< (Map.toUnfoldable :: Map _ _ -> Array _)

configDocMissingErrorMessage :: String
configDocMissingErrorMessage = Array.fold
  [ "This operation requires a YAML config document, but none was found in the environment. "
  , "This is an internal error. Please open an issue at https://github.com/purescript/spago/issues"
  ]

addPublishLocationToConfig :: YamlDoc Core.Config -> Location -> Effect Unit
addPublishLocationToConfig doc loc =
  runEffectFn2 addPublishLocationToConfigImpl doc (CJ.encode Core.publishLocationCodec loc)

type OwnerJS = { public :: String, keytype :: String, id :: Nullable String }

addOwner :: forall m. MonadAff m => LocalPath -> YamlDoc Core.Config -> Owner -> m Unit
addOwner configPath doc (Owner { id, keytype, public }) = do
  liftEffect $ runEffectFn2 addOwnerImpl doc { keytype, public, id: Nullable.toNullable id }
  liftAff $ FS.writeYamlDocFile configPath doc

foreign import setPackageSetVersionInConfigImpl :: EffectFn2 (YamlDoc Core.Config) String Unit
foreign import addPackagesToConfigImpl :: EffectFn3 (YamlDoc Core.Config) Boolean (Array String) Unit
foreign import removePackagesFromConfigImpl :: EffectFn3 (YamlDoc Core.Config) Boolean (PackageName -> Boolean) Unit
foreign import addRangesToConfigImpl :: EffectFn2 (YamlDoc Core.Config) (Foreign.Object String) Unit
foreign import addPublishLocationToConfigImpl :: EffectFn2 (YamlDoc Core.Config) JSON Unit
foreign import addOwnerImpl :: EffectFn2 (YamlDoc Core.Config) OwnerJS Unit
foreign import migrateV1ConfigImpl :: forall a. YamlDoc a -> Nullable (YamlDoc Core.Config)
