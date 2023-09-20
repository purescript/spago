module Spago.Config
  ( BuildOptions
  , LockfileSettings(..)
  , Package(..)
  , PackageSet(..)
  , PackageMap
  , WithTestGlobs(..)
  , Workspace
  , WorkspacePackage
  , addPackagesToConfig
  , addRangesToConfig
  , findPackageSet
  , getPackageLocation
  , getWorkspacePackages
  , module Core
  , readWorkspace
  , sourceGlob
  ) where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Data.Array as Array
import Data.Codec.Argonaut.Record as CAR
import Data.Foldable as Foldable
import Data.HTTP.Method as Method
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.String (Pattern(..))
import Data.String as String
import Dodo as Log
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object as Foreign
import Node.Path as Path
import Record as Record
import Registry.Foreign.FastGlob as Glob
import Registry.Internal.Codec as Internal.Codec
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Spago.Core.Config as Core
import Spago.FS as FS
import Spago.Git as Git
import Spago.Lock (Lockfile)
import Spago.Lock as Lock
import Spago.Paths as Paths
import Spago.Purs (PursEnv)
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
  , lockfile :: LockfileSettings
  }

type BuildOptions =
  { output :: Maybe FilePath
  , pedanticPackages :: Boolean
  , censorBuildWarnings :: Maybe Core.CensorBuildWarnings
  , censorCodes :: Maybe (NonEmptySet String)
  , filterCodes :: Maybe (NonEmptySet String)
  , statVerbosity :: Maybe Core.StatVerbosity
  , showSource :: Maybe Core.ShowSourceCode
  , strict :: Maybe Boolean
  , persistWarnings :: Maybe Boolean
  }

data LockfileSettings
  = UseLockfile Lockfile
  | GenerateLockfile
  | SkipLockfile

derive instance Eq LockfileSettings

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

data PackageSet
  = PackageSet PackageMap
  | Registry PackageMap

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
readWorkspace :: forall a. Maybe PackageName -> Spago (Git.GitEnv a) Workspace
readWorkspace maybeSelectedPackage = do
  logInfo "Reading Spago workspace configuration..."

  -- First try to read the config in the root. It _has_ to contain a workspace
  -- configuration, or we fail early.
  { workspace, package: maybePackage, workspaceDoc } <- Core.readConfig "spago.yaml" >>= case _ of
    Left err -> die $ "Couldn't parse Spago config, error:\n  " <> err
    Right { yaml: { workspace: Nothing } } -> die
      [ "Your spago.yaml doesn't contain a workspace section."
      , "See the relevant documentation here: https://github.com/purescript/spago#the-workspace"
      ]
    Right { yaml: { workspace: Just workspace, package }, doc } -> pure { workspace, package, workspaceDoc: doc }

  lockfile <- FS.exists "spago.lock" >>= case _ of
    true -> liftAff (FS.readYamlFile Lock.lockfileCodec "spago.lock") >>= case _ of
      Left error -> die $ "Your project contains a spago.lock file, but it cannot be decoded:\n" <> error
      Right contents
        | workspace.lock == Just false -> die "Your workspace specifies 'lock: false', but there is a spago.lock file in the workspace."
        | otherwise -> do
            -- TODO: here figure out if the lockfile is still valid by checking if:
            -- - the package set section of the workspace is the same
            -- - the dependencies of each package are the same
            pure (UseLockfile contents)
    false
      -- If the user specifies lock: true then we always create a lockfile.
      | workspace.lock == Just true -> pure GenerateLockfile
      -- If the user specifies lock: false then we always skip the lockfile.
      | workspace.lock == Just false -> pure SkipLockfile
      -- If the user does not set the 'lock' field then we defer to whether or
      -- not they are using a package set.
      | otherwise -> case workspace.package_set of
          Nothing -> pure GenerateLockfile
          Just _ -> pure SkipLockfile

  -- We try to figure out if the root package has tests - look for test sources
  rootPackageHasTests <- FS.exists "test"

  -- Then gather all the spago other configs in the tree.
  { succeeded: otherConfigPaths, failed, ignored } <- do
    result <- liftAff $ Glob.match' Paths.cwd [ "**/spago.yaml" ] { ignore: [ ".spago", "spago.yaml" ] }
    -- If a file is gitignored then we don't include it as a package
    let
      filterGitignored path = do
        Git.isIgnored path >>= case _ of
          true -> pure $ Left path
          false -> pure $ Right path
    { right: newSucceeded, left: ignored } <- partitionMap identity <$> traverse filterGitignored result.succeeded
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

  let
    workspacePackages = Map.fromFoldable $ configsNoWorkspaces <> case maybePackage of
      Nothing -> []
      Just package -> [ Tuple package.name { path: "./", package, doc: workspaceDoc, hasTests: rootPackageHasTests } ]

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

  -- Read in the package database
  { compatibleCompiler, remotePackageSet } <- case workspace.package_set of
    Nothing -> do
      logDebug "Did not find a package set in your config, using Registry solver"
      pure
        { compatibleCompiler: Core.widestRange
        , remotePackageSet: Nothing
        }
    Just (Core.SetFromRegistry { registry: v }) -> do
      logDebug "Reading the package set from the Registry repo..."
      let packageSetPath = Path.concat [ Paths.registryPath, "package-sets", Version.print v <> ".json" ]
      liftAff (FS.readJsonFile remotePackageSetCodec packageSetPath) >>= case _ of
        Left err -> die $ "Couldn't read the package set: " <> err
        Right (RemotePackageSet registryPackageSet) -> do
          logInfo "Read the package set from the registry"
          pure
            { compatibleCompiler: Range.caret registryPackageSet.compiler
            , remotePackageSet: Just registryPackageSet.packages
            }
    Just (Core.SetFromUrl { url: rawUrl, hash: maybeHash }) -> do
      -- If there is a hash then we look up in the CAS, if not we fetch stuff, compute a hash and store it there
      let
        fetchPackageSet = do
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
      result <- case maybeHash of
        Just hash -> readPackageSetFromHash hash >>= case _ of
          Left err -> do
            logDebug $ show err
            fetchPackageSet
          Right r -> pure r
        Nothing -> do
          logWarn $ "Did not find a hash for your package set import, adding it to your config..."
          fetchPackageSet
      newHash <- writePackageSetToHash result
      logDebug $ "Package set hash: " <> Sha256.print newHash
      liftEffect $ updatePackageSetHashInConfig workspaceDoc newHash
      liftAff $ FS.writeYamlDocFile "spago.yaml" workspaceDoc
      pure
        { compatibleCompiler: Range.caret result.compiler
        , remotePackageSet: Just result.remotePackageSet
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
    packageSet =
      let
        localPackages = Map.union (map WorkspacePackage workspacePackages) extraPackages
      in
        case remotePackageSet of
          Nothing -> Registry localPackages
          Just set -> PackageSet $ Map.union localPackages (map fromRemotePackage set)

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
      , pedanticPackages: fromMaybe false (_.pedantic_packages =<< workspace.build_opts)
      , censorBuildWarnings: _.censor_warnings =<< workspace.build_opts
      , censorCodes: _.censor_codes =<< workspace.build_opts
      , filterCodes: _.filter_codes =<< workspace.build_opts
      , statVerbosity: _.stat_verbosity =<< workspace.build_opts
      , showSource: _.show_source =<< workspace.build_opts
      , strict: _.strict =<< workspace.build_opts
      , persistWarnings: _.persist_warnings =<< workspace.build_opts
      }

  pure
    { selected: maybeSelected
    , packageSet
    , compatibleCompiler
    , backend: workspace.backend
    , buildOptions
    , doc: workspaceDoc
    , workspaceConfig: workspace
    , rootPackage: maybePackage
    , lockfile
    }

getPackageLocation :: PackageName -> Package -> FilePath
getPackageLocation name = Paths.mkRelative <<< case _ of
  RegistryVersion v -> Path.concat [ Paths.localCachePackagesPath, PackageName.print name <> "-" <> Version.print v ]
  GitPackage p -> Path.concat [ Paths.localCachePackagesPath, PackageName.print name, p.ref ]
  LocalPackage p -> p.path
  WorkspacePackage { path } -> path

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

getWorkspacePackages :: PackageSet -> Array WorkspacePackage
getWorkspacePackages = Array.mapMaybe extractWorkspacePackage <<< Map.toUnfoldable <<< case _ of
  PackageSet m -> m
  Registry m -> m
  where
  extractWorkspacePackage = case _ of
    Tuple _ (WorkspacePackage p) -> Just p
    _ -> Nothing

type PackageSetResult = { compiler :: Version, remotePackageSet :: Map PackageName Core.RemotePackage }

packageSetResultCodec :: JsonCodec PackageSetResult
packageSetResultCodec = CAR.object "PackageSetResult"
  { compiler: Version.codec
  , remotePackageSet: Internal.Codec.packageMap Core.remotePackageCodec
  }

readPackageSetFromHash :: forall a. Sha256 -> Spago (LogEnv a) (Either String PackageSetResult)
readPackageSetFromHash hash = do
  hex <- liftEffect (shaToHex hash)
  let path = packageSetCachePath hex
  logDebug $ "Reading cached package set entry from " <> path
  FS.exists path >>= case _ of
    false -> pure $ Left $ "Did not find a package set cached with hash " <> Sha256.print hash
    true -> (liftAff $ FS.readJsonFile packageSetResultCodec path) >>= case _ of
      Left err -> pure $ Left $ "Error while reading cached package set " <> Sha256.print hash <> ": " <> err
      Right res -> pure $ Right res

writePackageSetToHash :: forall a. PackageSetResult -> Spago (LogEnv a) Sha256
writePackageSetToHash result = do
  let serialised = printJson packageSetResultCodec result
  hash <- liftEffect $ Sha256.hashString serialised
  hex <- liftEffect (shaToHex hash)
  FS.mkdirp packageSetsCachePath
  FS.writeTextFile (packageSetCachePath hex) serialised
  pure hash

packageSetsCachePath :: FilePath
packageSetsCachePath = Path.concat [ Paths.globalCachePath, "setsCAS" ]

packageSetCachePath :: HexString → String
packageSetCachePath (HexString hash) = Path.concat [ packageSetsCachePath, hash ]

foreign import updatePackageSetHashInConfigImpl :: EffectFn2 (YamlDoc Core.Config) String Unit

updatePackageSetHashInConfig :: YamlDoc Core.Config -> Sha256 -> Effect Unit
updatePackageSetHashInConfig doc sha = runEffectFn2 updatePackageSetHashInConfigImpl doc (Sha256.print sha)

foreign import addPackagesToConfigImpl :: EffectFn2 (YamlDoc Core.Config) (Array String) Unit

addPackagesToConfig :: YamlDoc Core.Config -> Array PackageName -> Effect Unit
addPackagesToConfig doc pkgs = runEffectFn2 addPackagesToConfigImpl doc (map PackageName.print pkgs)

foreign import addRangesToConfigImpl :: EffectFn2 (YamlDoc Core.Config) (Foreign.Object String) Unit

addRangesToConfig :: YamlDoc Core.Config -> Map PackageName Range -> Effect Unit
addRangesToConfig doc = runEffectFn2 addRangesToConfigImpl doc
  <<< Foreign.fromFoldable
  <<< map (\(Tuple name range) -> Tuple (PackageName.print name) (Core.printSpagoRange range))
  <<< (Map.toUnfoldable :: Map _ _ -> Array _)

findPackageSet :: forall a. Maybe Version -> Spago (PursEnv a) Version
findPackageSet maybeSet = do
  -- first we read in the list of sets
  let
    parseSetVersion str = Version.parse case String.stripSuffix (Pattern ".json") str of
      Nothing -> str
      Just v -> v
    packageSetsPath = Path.concat [ Paths.registryPath, "package-sets" ]
  { success: setVersions, fail: parseFailures } <- map (partitionEithers <<< map parseSetVersion) $ FS.ls packageSetsPath

  unless (Array.null parseFailures) do
    logDebug $ [ toDoc "Failed to parse some package-sets versions:" ] <> map (indent <<< toDoc <<< show) parseFailures

  case maybeSet of
    -- if our input param is in the list of sets just return that
    Just desiredSet -> case Array.find (_ == desiredSet) setVersions of
      Just _ -> pure desiredSet
      Nothing -> die $ [ toDoc $ "Could not find desired set " <> Version.print desiredSet <> " in the list of available set versions:" ]
        <> map (indent <<< toDoc <<< Version.print) (Array.sort setVersions)
    -- no set in input: read the compiler version, look through the latest set by major version until we match the compiler version
    Nothing -> do
      -- build an index from compiler version to latest set, only looking at major versions
      -- TODO: we should probably cache this, like it's done in the legacy package sets
      let
        readPackageSet setVersion = do
          logDebug "Reading the package set from the Registry repo..."
          let packageSetPath = Path.concat [ packageSetsPath, Version.print setVersion <> ".json" ]
          liftAff (FS.readJsonFile remotePackageSetCodec packageSetPath) >>= case _ of
            Left err -> die $ "Couldn't read the package set: " <> err
            Right (RemotePackageSet registryPackageSet) -> do
              logDebug $ "Read the package set " <> Version.print setVersion <> " from the registry"
              pure registryPackageSet
        accVersions index newSetVersion = do
          -- first thing we check if we already have the latest set for this major version
          let maybeResult = Foldable.find (\(Tuple _c s) -> Version.major s == Version.major newSetVersion) (Map.toUnfoldable index :: Array (Tuple Version Version))
          case maybeResult of
            -- We have a version with the same major. If it's higher then we can just replace it (because we know it supports the same compiler)
            -- If it's not, we just return the current index and move on
            Just (Tuple currentCompiler currentVersion) ->
              if newSetVersion > currentVersion then do
                logDebug $ "Updating to package set " <> Version.print newSetVersion <> " for compiler " <> Version.print currentCompiler
                pure (Map.insert currentCompiler newSetVersion index)
              else pure index
            -- Didn't find a version with the same major, so we could be supporting a different compiler here.
            -- Read the set, check what we have for the compiler it supports
            Nothing -> do
              packageSet <- readPackageSet newSetVersion
              logDebug $ "Inserting package set " <> Version.print newSetVersion <> " for compiler " <> Version.print packageSet.compiler
              pure $ Map.insert packageSet.compiler newSetVersion index
      index :: Map Version Version <- Array.foldM accVersions Map.empty setVersions
      logDebug $ [ "Package set index", printJson (Internal.Codec.versionMap Version.codec) index ]

      -- now check if the compiler version is in the index
      { purs } <- ask
      case Map.lookup purs.version index of
        Just v -> pure v
        -- TODO: well we could approximate with any minor version really? See old Spago:
        -- https://github.com/purescript/spago/blob/01eecf041851ca0fbced1d4f7147fcbdd8bf168d/src/Spago/PackageSet.hs#L66
        Nothing -> die $ [ toDoc $ "No package set is compatible with your compiler version " <> Version.print purs.version, toDoc "Compatible versions:" ]
          <> map (indent <<< toDoc <<< Version.print) (Array.fromFoldable $ Map.keys index)
