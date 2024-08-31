module Spago.Config
  ( BuildType(..)
  , Package(..)
  , PackageSet(..)
  , PackageMap
  , WithTestGlobs(..)
  , Workspace
  , WorkspaceBuildOptions
  , WorkspacePackage
  , addPackagesToConfig
  , addRangesToConfig
  , removePackagesFromConfig
  , rootPackageToWorkspacePackage
  , getPackageLocation
  , fileSystemCharEscape
  , getWorkspacePackages
  , getTopologicallySortedWorkspacePackages
  , module Core
  , readWorkspace
  , sourceGlob
  , setPackageSetVersionInConfig
  , workspacePackageToLockfilePackage
  , readConfig
  ) where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode as Unicode
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Enum as Enum
import Data.Graph as Graph
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.List (List(..))
import Data.Map as Map
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String (CodePoint, Pattern(..))
import Data.String as String
import Dodo as Log
import Effect.Aff as Aff
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Foreign.Object as Foreign
import Node.Path as Path
import Registry.Internal.Codec as Internal.Codec
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
import Spago.Paths as Paths
import Spago.Registry as Registry
import Spago.Yaml as Yaml

type Workspace =
  { selected :: Maybe WorkspacePackage
  , packageSet :: PackageSet
  , compatibleCompiler :: Range
  , backend :: Maybe Core.BackendConfig
  , buildOptions :: WorkspaceBuildOptions
  , doc :: YamlDoc Core.Config
  , workspaceConfig :: Core.WorkspaceConfig
  , rootPackage :: Maybe Core.PackageConfig
  }

type WorkspaceBuildOptions =
  { output :: Maybe FilePath
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
  { path :: FilePath
  , package :: Core.PackageConfig
  , doc :: YamlDoc Core.Config
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
  , configPath :: FilePath
  , packagePath :: FilePath
  }

type ReadWorkspaceOptions =
  { maybeSelectedPackage :: Maybe PackageName
  , pureBuild :: Boolean
  , migrateConfig :: Boolean
  }

type PrelimWorkspace =
  { backend :: Maybe Core.BackendConfig
  , buildOpts :: Maybe
                 { censorLibraryWarnings :: Maybe Core.CensorBuildWarnings
                 , output :: Maybe String
                 , statVerbosity :: Maybe Core.StatVerbosity
                 }
  , extraPackages :: Maybe (Map PackageName Core.ExtraPackage)
  , packageSet :: Maybe Core.SetAddress
  }

-- | Reads all the configurations in the tree and builds up the Map of local
-- | packages to be integrated in the package set
readWorkspace :: ∀ a. ReadWorkspaceOptions -> Spago (Registry.RegistryEnv a) Workspace
-- readWorkspace readWorkspaceOptions@{ maybeSelectedPackage, pureBuild, migrateConfig } = do
readWorkspace { maybeSelectedPackage, pureBuild, migrateConfig } = do
  logInfo "Reading spago.yaml..."

  let
    doMigrateConfig :: FilePath -> _ -> Spago (Registry.RegistryEnv _) Unit
    doMigrateConfig path config = do
      case migrateConfig, config.wasMigrated of
        true, true -> do
          logInfo $ "Migrating your " <> path <> " to the latest version..."
          liftAff $ FS.writeYamlDocFile path config.doc
        false, true -> logWarn $ "Your " <> path <> " is using an outdated format. Run Spago with the --migrate flag to update it to the latest version."
        _, false -> pure unit

  let
    higherPaths :: List FilePath
    higherPaths = Array.toUnfoldable $ Paths.toGitSearchPath Paths.cwd

    checkForWorkspace :: forall b. FilePath
      -> Spago (LogEnv b) (Maybe PrelimWorkspace)
    checkForWorkspace config = do
      logDebug $ "Checking for workspace: " <> config
      result <- map (map (\y -> y.yaml)) $ readConfig config
      case result of
        Left _ -> pure Nothing
        Right { workspace: Nothing } -> pure Nothing
        Right { workspace: Just ws } -> pure (Just ws)

    searchHigherPaths :: forall b. List FilePath -> Spago (LogEnv b) (Maybe (Tuple FilePath PrelimWorkspace))
    searchHigherPaths Nil = pure Nothing
    searchHigherPaths (path : otherPaths) = do
      mGitRoot :: Maybe String <- map Array.head $ liftAff $ Glob.findGitGlob path
      case mGitRoot of
        Nothing -> do
          logDebug "No project root (.git) found at: "
          logDebug path
        Just gitRoot -> do
          logInfo "Project root (.git) found at: "
          logInfo $ path <> gitRoot
      mSpagoYaml :: Maybe String <- map (map (\yml -> path <> yml)) $ map Array.head $ liftAff $ Glob.gitignoringGlob path [ "./spago.yaml" ]

      case Tuple mSpagoYaml mGitRoot of
        Tuple Nothing Nothing -> searchHigherPaths otherPaths
        Tuple Nothing (Just gitRoot) -> do
          -- directory containing .git assumed to be the root of the project;
          -- do not search up the file tree further than this
          logInfo $ "No Spago workspace found in any directory up to project root: " <> path <> gitRoot
          pure Nothing
        Tuple (Just spagoYaml) (Just gitRoot) -> do
          mWorkspace :: Maybe PrelimWorkspace <- checkForWorkspace spagoYaml
          case mWorkspace of
            Nothing -> do
              -- directory containing .git assumed to be the root of the project;
              -- do not search up the file tree further than this
              logInfo $ "No Spago workspace found in any directory up to project root: " <> path <> gitRoot
              pure Nothing
            Just ws -> do
              logInfo spagoYaml
              pure (map (\p -> (Tuple p ws)) (String.stripSuffix (Pattern "spago.yaml") spagoYaml))
        Tuple (Just spagoYaml) Nothing -> do
          mWorkspace :: Maybe PrelimWorkspace <- checkForWorkspace spagoYaml
          case mWorkspace of
            Nothing -> searchHigherPaths otherPaths
            Just ws -> do
              logInfo spagoYaml
              pure (map (\p -> (Tuple p ws)) (String.stripSuffix (Pattern "spago.yaml") spagoYaml))

  -- First try to read the config in the root.
  -- Else, look for a workspace in parent directories.
  -- Else fail.
  { workspace, package: maybePackage, workspaceDoc } <- readConfig "spago.yaml" >>= case _ of
    Left errLines ->
      die
        [ toDoc "Couldn't parse Spago config, error:"
        , Log.break
        , indent $ toDoc errLines
        , Log.break
        , toDoc "The configuration file help can be found here https://github.com/purescript/spago#the-configuration-file"
        ]
    Right config@{ yaml: { workspace: Nothing, package }, doc } -> do
      logInfo "Looking for Spago workspace configuration higher in the filesystem."
      logInfo $ "Search limited to " <> show Paths.gitSearchDepth <> " levels, or project root (.git)..."
      mHigherWorkspace <- searchHigherPaths higherPaths
      case mHigherWorkspace of
        Nothing ->
          die
          [ "No workspace definition found in this directory"
          , "or in any directory up to root of project."
          , "Root determined by '.git' file."
          , "See the relevant documentation here: https://github.com/purescript/spago#the-workspace"
          ]
        Just (Tuple higherWorkspacePath higherWorkspace) -> do
          logInfo "Found workspace definition in: "
          logInfo higherWorkspacePath
          -- TODO migrate workspace at higher directory?
          doMigrateConfig "spago.yaml" config
          pure { workspace: higherWorkspace, package, workspaceDoc: doc }
    Right config@{ yaml: { workspace: Just workspace, package }, doc } -> do
      doMigrateConfig "spago.yaml" config
      pure { workspace, package, workspaceDoc: doc }

  logDebug "Gathering all the spago configs lower in the filesystem..."
  otherLowerConfigPaths <- liftAff $ Glob.gitignoringGlob Paths.cwd [ "**/spago.yaml" ]
  unless (Array.null otherLowerConfigPaths) do
    logDebug $ [ toDoc "Found packages at these lower paths:", Log.indent $ Log.lines (map toDoc otherLowerConfigPaths) ]

  -- We read all of them in, and only read the package section, if any.
  let
    readWorkspaceConfig :: FilePath -> Spago (Registry.RegistryEnv _) (Either Docc ReadWorkspaceConfigResult)
    readWorkspaceConfig path = do
      maybeConfig <- readConfig path
      -- We try to figure out if this package has tests - look for test sources
      hasTests <- FS.exists (Path.concat [ Path.dirname path, "test" ])
      pure $ case maybeConfig of
        Left eLines -> Left $ toDoc
          [ toDoc $ "Could not read config at path " <> path
          , toDoc "Error was: "
          , indent $ toDoc eLines
          ]
        Right config -> do
          Right { config, hasTests, configPath: path, packagePath: Path.dirname path }

  { right: otherPackages, left: failedPackages } <- partitionMap identity <$> traverse readWorkspaceConfig otherLowerConfigPaths
  unless (Array.null failedPackages) do
    logWarn $ [ toDoc "Failed to read some configs:" ] <> failedPackages

  -- We prune any configs that use a different workspace.
  -- For reasoning, see https://github.com/purescript/spago/issues/951
  let configPathsWithWorkspaces = otherPackages # Array.mapMaybe \readResult -> readResult.packagePath <$ readResult.config.yaml.workspace
  unless (Array.null configPathsWithWorkspaces) do
    logDebug $ "Found these paths with workspaces: " <> show configPathsWithWorkspaces

  { right: configsNoWorkspaces, left: prunedConfigs } <-
    let
      fn { left, right } readResult@{ configPath, packagePath, hasTests, config } = do
        if Array.any (\p -> isJust $ String.stripPrefix (Pattern p) packagePath) configPathsWithWorkspaces then
          pure { right, left: Array.cons packagePath left }
        else
          case readResult.config.yaml.package of
            Nothing -> pure { right, left: Array.cons packagePath left }
            Just package -> do
              -- Note: we migrate configs only at this point - this is because we read a whole lot of them but we are
              -- supposed to ignore any subtrees that contain a different workspace, and those we don't want to migrate
              doMigrateConfig configPath config
              -- We store the path of the package, so we can treat it basically as a LocalPackage
              pure { left, right: Array.cons (Tuple package.name { package, hasTests, path: packagePath, doc: config.doc }) right }
    in
      Array.foldM fn { right: [], left: [] } otherPackages

  unless (Array.null prunedConfigs) do
    logDebug $ [ "Excluding configs that use a different workspace (directly or implicitly via parent directory's config):" ] <> Array.sort prunedConfigs

  rootPackage <- case maybePackage of
    Nothing -> pure []
    Just rootPackage -> do
      rootPackage' <- rootPackageToWorkspacePackage { rootPackage, workspaceDoc }
      pure [ Tuple rootPackage.name rootPackage' ]

  let workspacePackages = Map.fromFoldable $ configsNoWorkspaces <> rootPackage

  -- Select the package for spago to handle during the rest of the execution
  maybeSelected <- case maybeSelectedPackage of
    Nothing -> case Array.uncons (Map.toUnfoldable workspacePackages) of
      Nothing -> die "No valid packages found in the current project, halting."
      -- If there's only one package and it's not in the root we still select that
      Just { head: (Tuple packageName package), tail: [] } -> do
        logDebug $ "Selecting package " <> PackageName.print packageName <> " from " <> package.path
        pure (Just package)
      -- If no package has been selected and we have many packages, then we build all of them but select none
      _ -> pure Nothing
    Just name -> case Map.lookup name workspacePackages of
      Nothing -> die
        $ [ toDoc $ "Selected package " <> PackageName.print name <> " was not found in the local packages." ]
        <> case (Array.fromFoldable $ Map.keys workspacePackages) of
          [] ->
            [ toDoc "No available packages." ]
          pkgs ->
            case typoSuggestions PackageName.print name pkgs of
              [] -> [ toDoc "All available packages:", indent (toDoc pkgs) ]
              suggestions -> [ toDoc "Did you mean:", indent (toDoc suggestions) ]
      Just p ->
        pure (Just p)

  logDebug "Reading the lockfile..."
  maybeLockfileContents <- FS.exists "spago.lock" >>= case _ of
    false -> pure (Left "No lockfile found")
    true -> liftAff (FS.readYamlFile Lock.lockfileCodec "spago.lock") >>= case _ of
      Left error -> do
        logWarn
          [ "Your project contains a spago.lock file, but it cannot be decoded. Spago will generate a new one."
          , "Error was: " <> error
          ]
        pure (Left "Could not decode lockfile")
      -- Here we figure out if the lockfile is still up to date by having a quick look at the configurations:
      -- if they changed since the last write, then we need to regenerate the lockfile
      -- Unless! the user is passing the --pure flag, in which case we just use the lockfile
      Right contents -> case pureBuild, shouldComputeNewLockfile { workspace, workspacePackages } contents.workspace of
        true, _ -> do
          logDebug "Using lockfile because of --pure flag"
          pure (Right contents)
        false, true -> pure (Left "Lockfile is out of date")
        false, false -> do
          logDebug "Lockfile is up to date, using it"
          pure (Right contents)

  -- Read in the package database
  { offline } <- ask
  packageSetInfo <- case maybeLockfileContents, workspace.packageSet of
    _, Nothing -> do
      logDebug "Did not find a package set in your config, using Registry solver"
      pure Nothing

    -- If there's a lockfile we don't attempt to fetch the package set from the registry
    -- repo nor from the internet, since we already have the whole set right there
    Right lockfile, _ -> do
      logDebug "Found the lockfile, using the package set from there"
      pure lockfile.workspace.package_set

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
      liftAff (FS.readJsonFile remotePackageSetCodec path) >>= case _ of
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
        Online -> do
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
    extraPackages = map fromExtraPackage (fromMaybe Map.empty workspace.extraPackages)
    localPackagesOverlap = Set.intersection (Map.keys workspacePackages) (Map.keys extraPackages)
    buildType =
      let
        localPackages = Map.union (map WorkspacePackage workspacePackages) extraPackages
      in
        case packageSetInfo of
          Nothing -> RegistrySolverBuild localPackages
          Just info -> PackageSetBuild info $ Map.union localPackages (map fromRemotePackage info.content)
    packageSet = { buildType, lockfile: maybeLockfileContents }

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

  -- Figure out if we are selecting a single package or not
  case maybeSelected of
    Just selected -> do
      logSuccess $ "Selecting package to build: " <> PackageName.print selected.package.name
      logDebug $ "Package path: " <> selected.path
    Nothing -> do
      logSuccess
        [ toDoc $ "Selecting " <> show (Map.size workspacePackages) <> " packages to build:"
        , indent2 (toDoc (Set.toUnfoldable $ Map.keys workspacePackages :: Array PackageName))
        ]

  let
    buildOptions :: WorkspaceBuildOptions
    buildOptions =
      { output: _.output =<< workspace.buildOpts
      , censorLibWarnings: _.censorLibraryWarnings =<< workspace.buildOpts
      , statVerbosity: _.statVerbosity =<< workspace.buildOpts
      }

  pure
    { selected: maybeSelected
    , packageSet
    , compatibleCompiler: fromMaybe Core.widestRange $ map _.compiler packageSetInfo
    , backend: workspace.backend
    , buildOptions
    , doc: workspaceDoc
    , workspaceConfig: workspace
    , rootPackage: maybePackage
    }

rootPackageToWorkspacePackage
  :: forall m
   . MonadEffect m
  => { rootPackage :: Core.PackageConfig, workspaceDoc :: YamlDoc Core.Config }
  -> m WorkspacePackage
rootPackageToWorkspacePackage { rootPackage, workspaceDoc } = do
  hasTests <- liftEffect $ FS.exists "test"
  pure { path: "./", doc: workspaceDoc, package: rootPackage, hasTests }

workspacePackageToLockfilePackage :: WorkspacePackage -> Tuple PackageName Lock.WorkspaceLockPackage
workspacePackageToLockfilePackage { path, package } = Tuple package.name
  { path: withForwardSlashes path
  , core: { dependencies: package.dependencies, build_plan: mempty }
  , test: { dependencies: foldMap _.dependencies package.test, build_plan: mempty }
  }

shouldComputeNewLockfile :: { workspace :: Core.WorkspaceConfig, workspacePackages :: Map PackageName WorkspacePackage } -> Lock.WorkspaceLock -> Boolean
shouldComputeNewLockfile { workspace, workspacePackages } workspaceLock =
  -- the workspace packages should exactly match, except for the needed_by field, which is filled in during build plan construction
  ((workspacePackageToLockfilePackage >>> snd <$> workspacePackages) /= (eraseBuildPlan <$> workspaceLock.packages))
    -- and the extra packages should exactly match
    || (fromMaybe Map.empty workspace.extraPackages /= workspaceLock.extra_packages)
    -- and the package set address needs to match - we have no way to match the package set contents at this point, so we let it be
    || (workspace.packageSet /= map _.address workspaceLock.package_set)
    -- and the package set is not a local file - if it is then we always recompute the lockfile because we have no way to check if it's changed
    ||
      ( case workspace.packageSet of
          Just (Core.SetFromPath _) -> true
          _ -> false
      )
  where
  eraseBuildPlan = _ { core { build_plan = mempty }, test { build_plan = mempty } }

getPackageLocation :: PackageName -> Package -> FilePath
getPackageLocation name = Paths.mkRelative <<< case _ of
  RegistryVersion v -> Path.concat [ Paths.localCachePackagesPath, PackageName.print name <> "-" <> Version.print v ]
  GitPackage p -> Path.concat [ Paths.localCachePackagesPath, PackageName.print name, fileSystemCharEscape p.ref ]
  LocalPackage p -> p.path
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

sourceGlob :: WithTestGlobs -> PackageName -> Package -> Array String
sourceGlob withTestGlobs name package = map (\p -> Path.concat [ getPackageLocation name package, p ])
  case package of
    WorkspacePackage { hasTests } ->
      case hasTests, withTestGlobs of
        false, OnlyTestGlobs -> []
        false, _ -> [ srcGlob ]
        true, OnlyTestGlobs -> [ testGlob ]
        true, NoTestGlobs -> [ srcGlob ]
        true, WithTestGlobs -> [ srcGlob, testGlob ]
    GitPackage { subdir: Just s } -> [ Path.concat [ s, srcGlob ] ]
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

readConfig :: forall a. FilePath -> Spago (LogEnv a) (Either (Array String) ReadConfigResult)
readConfig path = do
  logDebug $ "Reading config from " <> path
  try (FS.readTextFile path) >>= case _ of
    Left err -> do
      logDebug $ "Could not read file " <> path <> ", error: " <> Aff.message err
      let replaceExt = map (_ <> ".yml") <<< String.stripSuffix (String.Pattern ".yaml")
      yml <- map join $ for (replaceExt path) \yml -> do
        hasYml <- FS.exists yml
        pure $
          if hasYml then
            Just yml
          else
            Nothing
      pure $ Left $ case path, yml of
        "spago.yaml", Nothing ->
          [ "Did not find `" <> path <> "`. Run `spago init` to initialize a new project." ]
        "spago.yaml", Just y ->
          [ "Did not find `" <> path <> "`. Spago's configuration files must end with `.yaml`, not `.yml`."
          , "Try renaming `" <> y <> "` to `" <> path <> "` or run `spago init` to initialize a new project."
          ]
        _, Nothing ->
          [ "Did not find `" <> path <> "`." ]
        _, Just y ->
          [ "Did not find `" <> path <> "`. Spago's configuration files must end with `.yaml`, not `.yml`."
          , "Try renaming `" <> y <> "` to `" <> path <> "`."
          ]
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

setPackageSetVersionInConfig :: forall m. MonadAff m => MonadEffect m => YamlDoc Core.Config -> Version -> m Unit
setPackageSetVersionInConfig doc version = do
  liftEffect $ runEffectFn2 setPackageSetVersionInConfigImpl doc (Version.print version)
  liftAff $ FS.writeYamlDocFile "spago.yaml" doc

addPackagesToConfig :: forall m. MonadAff m => FilePath -> YamlDoc Core.Config -> Boolean -> Array PackageName -> m Unit
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

foreign import setPackageSetVersionInConfigImpl :: EffectFn2 (YamlDoc Core.Config) String Unit
foreign import addPackagesToConfigImpl :: EffectFn3 (YamlDoc Core.Config) Boolean (Array String) Unit
foreign import removePackagesFromConfigImpl :: EffectFn3 (YamlDoc Core.Config) Boolean (PackageName -> Boolean) Unit
foreign import addRangesToConfigImpl :: EffectFn2 (YamlDoc Core.Config) (Foreign.Object String) Unit
foreign import migrateV1ConfigImpl :: forall a. YamlDoc a -> Nullable (YamlDoc Core.Config)
