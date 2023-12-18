module Spago.Config
  ( BuildOptions
  , BuildType(..)
  , Package(..)
  , PackageSet(..)
  , PackageMap
  , WithTestGlobs(..)
  , Workspace
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
  ) where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode as Unicode
import Data.Codec.Argonaut.Record as CAR
import Data.Enum as Enum
import Data.Graph as Graph
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String (CodePoint, Pattern(..))
import Data.String as String
import Dodo as Log
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Foreign.Object as Foreign
import Node.Path as Path
import Record as Record
import Registry.Foreign.FastGlob as Glob
import Registry.Internal.Codec as Internal.Codec
import Registry.PackageName as PackageName
import Registry.PackageSet as Registry.PackageSet
import Registry.Range as Range
import Registry.Version as Version
import Spago.Core.Config as Core
import Spago.FS as FS
import Spago.Git as Git
import Spago.Lock (Lockfile, PackageSetInfo)
import Spago.Lock as Lock
import Spago.Paths as Paths
import Spago.Registry as Registry
import Type.Proxy (Proxy(..))

type Workspace =
  { selected :: Maybe WorkspacePackage
  , packageSet :: PackageSet
  , compatibleCompiler :: Range
  , backend :: Maybe Core.BackendConfig
  , buildOptions :: BuildOptions
  , doc :: YamlDoc Core.Config
  , workspaceConfig :: Core.WorkspaceConfig
  , rootPackage :: Maybe Core.PackageConfig
  }

type BuildOptions =
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

legacyPackageSetCodec :: JsonCodec LegacyPackageSet
legacyPackageSetCodec =
  Profunctor.wrapIso LegacyPackageSet
    $ Internal.Codec.packageMap Core.legacyPackageSetEntryCodec

newtype RemotePackageSet = RemotePackageSet
  { compiler :: Version
  , packages :: Map PackageName Core.RemotePackage
  , version :: Version
  }

remotePackageSetCodec :: JsonCodec RemotePackageSet
remotePackageSetCodec = Profunctor.wrapIso RemotePackageSet $ CAR.object "PackageSet"
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
  , lockfile :: Maybe Lockfile
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

-- | Reads all the configurations in the tree and builds up the Map of local
-- | packages to be integrated in the package set
readWorkspace :: { maybeSelectedPackage :: Maybe PackageName, pureBuild :: Boolean } -> Spago (Registry.RegistryEnv _) Workspace
readWorkspace { maybeSelectedPackage, pureBuild } = do
  logInfo "Reading Spago workspace configuration..."

  -- First try to read the config in the root. It _has_ to contain a workspace
  -- configuration, or we fail early.
  { workspace, package: maybePackage, workspaceDoc } <- Core.readConfig "spago.yaml" >>= case _ of
    Left err -> die [ "Couldn't parse Spago config, error:\n  " <> err, "Run `spago init` to initialise a new project." ]
    Right { yaml: { workspace: Nothing } } -> die
      [ "Your spago.yaml doesn't contain a workspace section."
      , "See the relevant documentation here: https://github.com/purescript/spago#the-workspace"
      ]
    Right { yaml: { workspace: Just workspace, package }, doc } -> pure { workspace, package, workspaceDoc: doc }

  -- Then gather all the spago other configs in the tree.
  { succeeded: otherConfigPaths, failed, ignored } <- do
    result <- liftAff $ Glob.match' Paths.cwd [ "**/spago.yaml" ] { ignore: [ ".spago", "spago.yaml" ] }
    -- If a file is gitignored then we don't include it as a package
    let
      filterGitignored path = do
        Git.isIgnored path >>= case _ of
          true -> pure $ Left path
          false -> pure $ Right path
    { right: newSucceeded, left: ignored } <- partitionMap identity
      <$> parTraverseSpago filterGitignored result.succeeded
    pure { succeeded: newSucceeded, failed: result.failed, ignored }
  unless (Array.null otherConfigPaths) do
    logDebug $ [ toDoc "Found packages at these paths:", Log.indent $ Log.lines (map toDoc otherConfigPaths) ]
  unless (Array.null failed) do
    logDebug $ "Failed to sanitise some of the glob matches: " <> show failed
  unless (Array.null ignored) do
    logDebug $ "Ignored some of the glob matches as they are gitignored: " <> show ignored

  -- We read all of them in, and only read the package section, if any.
  let
    readWorkspaceConfig path = do
      maybeConfig <- Core.readConfig path
      -- We try to figure out if this package has tests - look for test sources
      hasTests <- FS.exists (Path.concat [ Path.dirname path, "test" ])
      pure $ case maybeConfig of
        Left e -> Left $ "Could not read config at path " <> path <> "\nError was: " <> e
        Right { yaml: { package: Nothing } } -> Left $ "No package found for config at path: " <> path
        Right { yaml: { package: Just package, workspace: configWorkspace }, doc } -> do
          -- We store the path of the package, so we can treat it basically as a LocalPackage
          Right $ Tuple package.name { path: Path.dirname path, package, configWorkspace, doc, hasTests }
  { right: otherPackages, left: failedPackages } <- partitionMap identity <$> traverse readWorkspaceConfig otherConfigPaths

  unless (Array.null failedPackages) do
    logWarn $ [ "Failed to read some configs:" ] <> failedPackages

  -- We prune any configs that use a different workspace.
  -- For reasoning, see https://github.com/purescript/spago/issues/951
  let
    configPathsWithWorkspaces = Array.mapMaybe (\(Tuple _ configParts) -> configParts.path <$ configParts.configWorkspace) otherPackages
    { right: configsNoWorkspaces, left: prunedConfigs } = otherPackages # partitionMap \config -> do
      let configPath = (snd config).path
      if Array.any (\p -> isJust $ String.stripPrefix (Pattern p) configPath) configPathsWithWorkspaces then
        Left configPath
      else
        Right $ config <#> Record.delete (Proxy :: _ "configWorkspace")

  -- TODO: this should be a logwarning, and there should be a test
  unless (Array.null prunedConfigs) do
    logDebug $ [ "Excluding configs that use a different workspace (directly or implicitly via parent directory's config):" ] <> Array.sort failedPackages

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
          [] -> [ toDoc "No available packages." ]
          pkgs -> [ toDoc "Available packages:", indent (toDoc pkgs) ]
      Just p -> pure (Just p)

  maybeLockfileContents <- FS.exists "spago.lock" >>= case _ of
    true -> liftAff (FS.readYamlFile Lock.lockfileCodec "spago.lock") >>= case _ of
      Left error -> do
        logWarn
          [ "Your project contains a spago.lock file, but it cannot be decoded. Spago will generate a new one."
          , "Error was: " <> error
          ]
        pure Nothing
      -- Here we figure out if the lockfile is still up to date by having a quick look at the configurations:
      -- if they changed since the last write, then we need to regenerate the lockfile
      -- Unless! the user is passing the --pure flag, in which case we just use the lockfile
      Right contents -> case pureBuild, shouldComputeNewLockfile { workspace, workspacePackages } contents.workspace of
        false, false -> pure Nothing
        _, _ -> pure (Just contents)
    false -> pure Nothing

  -- Read in the package database
  { offline } <- ask
  packageSetInfo <- case maybeLockfileContents, workspace.package_set of
    _, Nothing -> do
      logDebug "Did not find a package set in your config, using Registry solver"
      pure Nothing

    -- If there's a lockfile we don't attempt to fetch the package set from the registry
    -- repo nor from the internet, since we already have the whole set right there
    Just lockfile, _ -> do
      logDebug "Found lockfile, using the package set from there"
      pure lockfile.workspace.package_set

    Nothing, Just address@(Core.SetFromRegistry { registry: v }) -> do
      logDebug "Reading the package set from the Registry repo..."
      (Registry.PackageSet.PackageSet registryPackageSet) <- Registry.readPackageSet v
      logDebug "Read the package set from the Registry repo"
      pure $ Just
        { content: map Core.RemoteRegistryVersion registryPackageSet.packages
        , address
        , compiler: Range.caret registryPackageSet.compiler
        }

    Nothing, Just address@(Core.SetFromPath { path }) -> do
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

    Nothing, Just address@(Core.SetFromUrl { url: rawUrl }) -> do
      result <- case offline of
        Offline -> die "You are offline, but the package set is not cached locally. Please connect to the internet and try again."
        Online -> do
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
    extraPackages = map fromExtraPackage (fromMaybe Map.empty workspace.extra_packages)
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
    buildOptions :: BuildOptions
    buildOptions =
      { output: _.output =<< workspace.build_opts
      , censorLibWarnings: _.censor_library_warnings =<< workspace.build_opts
      , statVerbosity: _.stat_verbosity =<< workspace.build_opts
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
  { path
  , dependencies: package.dependencies
  , test_dependencies: foldMap _.dependencies package.test
  , build_plan: mempty -- Note: this is filled in later
  }

shouldComputeNewLockfile :: { workspace :: Core.WorkspaceConfig, workspacePackages :: Map PackageName WorkspacePackage } -> Lock.WorkspaceLock -> Boolean
shouldComputeNewLockfile { workspace, workspacePackages } workspaceLock =
  -- the workspace packages should exactly match, except for the needed_by field, which is filled in during build plan construction
  (map (workspacePackageToLockfilePackage >>> snd) workspacePackages == (map (_ { build_plan = mempty }) workspaceLock.packages))
    -- and the extra packages should exactly match
    && (fromMaybe Map.empty workspace.extra_packages == workspaceLock.extra_packages)
    -- and the package set address needs to match - we have no way to match the package set contents at this point, so we let it be
    && (workspace.package_set == map _.address workspaceLock.package_set)

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

type PackageSetResult = { compiler :: Version, remotePackageSet :: Map PackageName Core.RemotePackage }

foreign import setPackageSetVersionInConfigImpl :: EffectFn2 (YamlDoc Core.Config) String Unit

setPackageSetVersionInConfig :: forall m. MonadAff m => MonadEffect m => YamlDoc Core.Config -> Version -> m Unit
setPackageSetVersionInConfig doc version = do
  liftEffect $ runEffectFn2 setPackageSetVersionInConfigImpl doc (Version.print version)
  liftAff $ FS.writeYamlDocFile "spago.yaml" doc

foreign import addPackagesToConfigImpl :: EffectFn3 (YamlDoc Core.Config) Boolean (Array String) Unit

addPackagesToConfig :: YamlDoc Core.Config -> Boolean -> Array PackageName -> Effect Unit
addPackagesToConfig doc isTest pkgs = runEffectFn3 addPackagesToConfigImpl doc isTest (map PackageName.print pkgs)

foreign import removePackagesFromConfigImpl :: EffectFn3 (YamlDoc Core.Config) Boolean (PackageName -> Boolean) Unit

removePackagesFromConfig :: YamlDoc Core.Config -> Boolean -> NonEmptySet PackageName -> Effect Unit
removePackagesFromConfig doc isTest pkgs = runEffectFn3 removePackagesFromConfigImpl doc isTest (flip NonEmptySet.member pkgs)

foreign import addRangesToConfigImpl :: EffectFn2 (YamlDoc Core.Config) (Foreign.Object String) Unit

addRangesToConfig :: YamlDoc Core.Config -> Map PackageName Range -> Effect Unit
addRangesToConfig doc = runEffectFn2 addRangesToConfigImpl doc
  <<< Foreign.fromFoldable
  <<< map (\(Tuple name range) -> Tuple (PackageName.print name) (Core.printSpagoRange range))
  <<< (Map.toUnfoldable :: Map _ _ -> Array _)
