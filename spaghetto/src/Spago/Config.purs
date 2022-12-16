module Spago.Config
  ( BackendConfig
  , BundleConfig
  , BundlePlatform(..)
  , BundleType(..)
  , Config
  , Dependencies(..)
  , ExtraPackage(..)
  , GitPackage
  , LocalPackage
  , Package(..)
  , PackageConfig
  , PackageSet
  , PublishConfig
  , RemotePackage(..)
  , RunConfig
  , SetAddress(..)
  , TestConfig
  , Workspace
  , WorkspaceConfig
  , WorkspacePackage
  , addPackagesToConfig
  , findPackageSet
  , getPackageLocation
  , getWorkspacePackages
  , parseBundleType
  , parsePlatform
  , readConfig
  , readWorkspace
  , sourceGlob
  , toManifest
  ) where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core as Core
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either as Either
import Data.Foldable as Foldable
import Data.HTTP.Method as Method
import Data.Map as Map
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Dodo as Log
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.FastGlob as Glob
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.SPDX (License)
import Node.Path as Path
import Parsing as Parsing
import Registry.Hash (Sha256)
import Registry.Hash as Registry.Hash
import Registry.Json as RegistryJson
import Registry.Legacy.PackageSet as Registry.PackageSet
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Prelude (partitionEithers)
import Registry.Schema (Location, Manifest(..))
import Registry.Version (Range, Version)
import Registry.Version as Registry.Version
import Registry.Version as Version
import Spago.FS as FS
import Spago.Git as Git
import Spago.Paths as Paths
import Spago.Purs (PursEnv)
import Spago.Yaml (class ToYaml, YamlDoc, (.:), (.:?))
import Spago.Yaml as Yaml

type Config =
  { package :: Maybe PackageConfig
  , workspace :: Maybe WorkspaceConfig
  }

type PackageConfig =
  { name :: PackageName
  , description :: Maybe String
  , dependencies :: Dependencies
  , bundle :: Maybe BundleConfig
  , run :: Maybe RunConfig
  , test :: Maybe TestConfig
  , publish :: Maybe PublishConfig
  }

type PublishConfig =
  { version :: Maybe Version
  , license :: Maybe License
  , location :: Maybe Location
  } -- FIXME: implement publishing

type RunConfig =
  { main :: Maybe String
  , execArgs :: Maybe (Array String)
  }

type TestConfig =
  { main :: String
  , execArgs :: Maybe (Array String)
  , dependencies :: Dependencies
  }

type BackendConfig =
  { cmd :: String
  , args :: Maybe (Array String)
  }

type WorkspaceConfig =
  { set :: Maybe SetAddress
  , extra_packages :: Maybe (Map PackageName ExtraPackage)
  , backend :: Maybe BackendConfig
  , output :: Maybe FilePath
  }

type Workspace =
  { selected :: Maybe WorkspacePackage
  , packageSet :: PackageSet
  , compatibleCompiler :: Range
  , backend :: Maybe BackendConfig
  , output :: Maybe String
  , doc :: YamlDoc Config
  }

data ExtraPackage
  = ExtraLocalPackage LocalPackage
  | ExtraRemotePackage RemotePackage

instance ToYaml ExtraPackage where
  encode (ExtraLocalPackage lp) = Yaml.encode lp
  encode (ExtraRemotePackage rp) = Yaml.encode rp

  decode yaml = decodeLocal <|> decodeRemote
    where
    decodeLocal = map ExtraLocalPackage (Yaml.decode yaml)
    decodeRemote = map ExtraRemotePackage (Yaml.decode yaml)

fromExtraPackage :: ExtraPackage -> Package
fromExtraPackage = case _ of
  ExtraLocalPackage lp -> LocalPackage lp
  ExtraRemotePackage rp -> fromRemotePackage rp

fromRemotePackage :: RemotePackage -> Package
fromRemotePackage = case _ of
  RemoteGitPackage p -> GitPackage p
  RemoteRegistryVersion v -> RegistryVersion v
  RemoteLegacyPackage e -> GitPackage
    { git: e.repo
    , ref: unwrap e.version
    , subdir: Nothing
    , dependencies: Just $ Dependencies $ Map.fromFoldable $ map (\p -> Tuple p Nothing) e.dependencies
    }

data RemotePackage
  = RemoteGitPackage GitPackage
  | RemoteRegistryVersion Version
  | RemoteLegacyPackage Registry.PackageSet.LegacyPackageSetEntry

derive instance Eq RemotePackage

instance Show RemotePackage where
  show = case _ of
    RemoteRegistryVersion v -> show v
    RemoteGitPackage p -> show p
    RemoteLegacyPackage p -> show p

newtype RemotePackageSet = RemotePackageSet
  { compiler :: Version
  , packages :: Map PackageName RemotePackage
  }

instance RegistryJson.RegistryJson RemotePackage where
  encode (RemoteRegistryVersion v) = RegistryJson.encode v
  encode (RemoteGitPackage p) = RegistryJson.encode p
  encode (RemoteLegacyPackage p) = RegistryJson.encode p

  decode json =
    (Core.caseJsonString (Left "Expected String RemotePackage") decodeVersion json)
      <|> (Core.caseJsonObject (Left "Expected Object RemotePackage") (\j -> decodePkg j <|> decodeLegacyPkg j) json)
    where
    decodePkg obj = do
      dependencies <- obj RegistryJson..:? "dependencies"
      subdir <- obj RegistryJson..:? "subdir"
      git <- obj RegistryJson..: "git"
      ref <- obj RegistryJson..: "ref"
      pure (RemoteGitPackage { dependencies, git, ref, subdir })
    decodeLegacyPkg obj = do
      dependencies <- obj RegistryJson..: "dependencies"
      repo <- obj RegistryJson..: "repo"
      version <- obj RegistryJson..: "version"
      pure (RemoteLegacyPackage { dependencies, repo, version })
    decodeVersion = map RemoteRegistryVersion <<< (RegistryJson.fromEncodableString :: String -> Either String Version)

instance ToYaml RemotePackage where
  encode = RegistryJson.encode
  decode = RegistryJson.decode

derive instance Newtype RemotePackageSet _
derive newtype instance Eq RemotePackageSet
derive newtype instance Show RemotePackageSet

instance RegistryJson.RegistryJson RemotePackageSet where
  encode (RemotePackageSet plan) = RegistryJson.encode plan
  decode = map RemotePackageSet <<< RegistryJson.decode

type PackageSet = Map PackageName Package

type WorkspacePackage =
  { path :: FilePath
  , package :: PackageConfig
  , doc :: YamlDoc Config
  , hasTests :: Boolean
  }

data Package
  = RegistryVersion Version
  | GitPackage GitPackage
  | LocalPackage LocalPackage
  | WorkspacePackage WorkspacePackage

instance Show Package where
  show = case _ of
    RegistryVersion v -> show v
    GitPackage p -> show p
    LocalPackage p -> show p
    WorkspacePackage p -> show p

type LocalPackage = { path :: FilePath }

type GitPackage =
  { git :: String
  , ref :: String
  , subdir :: Maybe FilePath -- TODO: document that this is possible
  , dependencies :: Maybe Dependencies -- TODO document that this is possible
  }

newtype Dependencies = Dependencies (Map PackageName (Maybe Range))

derive instance Eq Dependencies
derive instance Newtype Dependencies _

instance Semigroup Dependencies where
  append (Dependencies d1) (Dependencies d2) = Dependencies $ Map.unionWith
    ( case _, _ of
        Nothing, Nothing -> Nothing
        Just r, Nothing -> Just r
        Nothing, Just r -> Just r
        Just r1, Just r2 -> Version.intersect r1 r2
    )
    d1
    d2

instance Monoid Dependencies where
  mempty = Dependencies (Map.empty)

instance Show Dependencies where
  show (Dependencies ds) = show ds

instance ToYaml Dependencies where
  encode (Dependencies deps) = Core.fromArray
    $ Array.fromFoldable
    $ Map.mapMaybeWithKey
        ( \name maybeRange -> Just $ case maybeRange of
            Nothing -> Core.fromString (PackageName.print name)
            Just range -> Core.jsonSingletonObject (PackageName.print name) (Core.fromString (Registry.Version.printRange range))
        )
        deps
  decode =
    let
      decodePkgWithRange :: Object Core.Json -> Either String (Tuple PackageName (Maybe Range))
      decodePkgWithRange obj = do
        o <- traverse Yaml.decode obj
        let maybeTuple = Object.toAscUnfoldable o
        case maybeTuple of
          Nothing -> Left "Expected Object here"
          Just (Tuple rawPkg rawRange) -> do
            pkgName <- lmap Parsing.parseErrorMessage $ PackageName.parse rawPkg
            range <- lmap Parsing.parseErrorMessage $ Registry.Version.parseRange Registry.Version.Lenient rawRange
            Right (Tuple pkgName (Just range))

      decodePkg :: String -> Either String (Tuple PackageName (Maybe Range))
      decodePkg str = case PackageName.parse str of
        Left e -> Left (Parsing.parseErrorMessage e)
        Right p -> Right (Tuple p Nothing)
    in
      Core.caseJsonArray (Left "Expected Array of Dependencies")
        ( \arrayOfJson -> map
            (Dependencies <<< Map.fromFoldable)
            ( for arrayOfJson \el ->
                (Core.caseJsonString (Left "Expected String Dependency") decodePkg el)
                  <|> (Core.caseJsonObject (Left "Expected Object Dependency") decodePkgWithRange el)
            )
        )

instance RegistryJson.RegistryJson Dependencies where
  encode = Yaml.encode
  decode = Yaml.decode

data SetAddress
  = SetFromRegistry { registry :: Version }
  | SetFromUrl { url :: String, hash :: Maybe Sha256 }

instance Show SetAddress where
  show (SetFromRegistry s) = show s
  show (SetFromUrl s) = show s

instance ToYaml SetAddress where
  encode (SetFromRegistry r) = Yaml.encode r
  encode (SetFromUrl u) = Yaml.encode u

  decode yaml = do
    obj <- Yaml.decode yaml
    registry <- obj .:? "registry"
    case registry of
      Just v -> pure (SetFromRegistry { registry: v })
      Nothing -> do
        url <- obj .: "url"
        hash <- obj .:? "hash"
        pure (SetFromUrl { url, hash })

type BundleConfig =
  { minify :: Maybe Boolean
  , module :: Maybe String
  , outfile :: Maybe FilePath
  , platform :: Maybe BundlePlatform
  , type :: Maybe BundleType
  }

data BundlePlatform = BundleNode | BundleBrowser

instance Show BundlePlatform where
  show = case _ of
    BundleNode -> "node"
    BundleBrowser -> "browser"

parsePlatform :: String -> Maybe BundlePlatform
parsePlatform = case _ of
  "node" -> Just BundleNode
  "browser" -> Just BundleBrowser
  _ -> Nothing

instance ToYaml BundlePlatform where
  encode = Core.fromString <<< show
  decode = Either.note "Expected BundlePlatform" <<< parsePlatform <=< Yaml.decode

-- | This is the equivalent of "WithMain" in the old Spago.
-- App bundles with a main fn, while Module does not include a main.
data BundleType = BundleApp | BundleModule

instance Show BundleType where
  show = case _ of
    BundleApp -> "app"
    BundleModule -> "module"

parseBundleType :: String -> Maybe BundleType
parseBundleType = case _ of
  "app" -> Just BundleApp
  "module" -> Just BundleModule
  _ -> Nothing

instance ToYaml BundleType where
  encode = Core.fromString <<< show
  decode = Either.note "Expected BundleType" <<< parseBundleType <=< Yaml.decode

readConfig :: forall a. FilePath -> Spago (LogEnv a) (Either String { doc :: YamlDoc Config, yaml :: Config })
readConfig path = do
  logDebug $ "Reading config from " <> path
  FS.exists path >>= case _ of
    false -> pure (Left $ "Did not find " <> path <> " file. Run `spago init` to initialise a new project.")
    true -> liftAff $ Yaml.readYamlDocFile path

-- | Reads all the configurations in the tree and builds up the Map of local
-- | packages to be integrated in the package set
readWorkspace :: forall a. Maybe PackageName -> Spago (Git.GitEnv a) Workspace
readWorkspace maybeSelectedPackage = do
  logInfo "Reading Spago workspace configuration..."
  -- First try to read the config in the root. It _has_ to contain a workspace
  -- configuration, or we fail early.
  { workspace, package: maybePackage, workspaceDoc } <- readConfig "spago.yaml" >>= case _ of
    Left err -> die $ "Couldn't parse Spago config, error:\n  " <> err
    Right { yaml: { workspace: Nothing } } -> die $ "Your spago.yaml doesn't contain a workspace section" -- TODO refer to the docs
    Right { yaml: { workspace: Just workspace, package }, doc } -> pure { workspace, package, workspaceDoc: doc }

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
  -- TODO: we should probably not include at all the configs that contain a
  -- workspace configuration, maybe even "prune the tree" whenever we find one
  let
    readWorkspaceConfig path = do
      maybeConfig <- readConfig path
      -- We try to figure out if this package has tests - look for test sources
      hasTests <- FS.exists (Path.concat [ Path.dirname path, "test" ])
      pure $ case maybeConfig of
        Left e -> Left $ "Could not read config at path " <> path <> "\nError was: " <> e
        Right { yaml: { package: Nothing } } -> Left $ "No package found for config at path: " <> path
        Right { yaml: { package: Just package }, doc } -> do
          -- We store the path of the package, so we can treat is basically as a LocalPackage
          Right $ Tuple package.name { path: Path.dirname path, package, doc, hasTests }
  { right: otherPackages, left: failedPackages } <- partitionMap identity <$> traverse readWorkspaceConfig otherConfigPaths

  unless (Array.null failedPackages) do
    logWarn $ [ "Failed to read some configs:" ] <> failedPackages

  let
    workspacePackages = Map.fromFoldable $ otherPackages <> case maybePackage of
      Nothing -> []
      Just package -> [ Tuple package.name { path: "./", package, doc: workspaceDoc, hasTests: rootPackageHasTests } ]

  -- Select the package for spago to handle during the rest of the execution
  maybeSelected <- case maybeSelectedPackage of
    Nothing -> case Array.uncons (Map.toUnfoldable workspacePackages) of
      Nothing -> die "No valid packages found in the current project, halting."
      -- If there's only one package and it's not in the root we still select that
      Just { head: (Tuple packageName package), tail: [] } -> do
        logDebug $ "Selecting package " <> show packageName <> " from " <> package.path
        pure (Just package)
      -- If no package has been selected and we have many packages, then we build all of them but select none
      _ -> pure Nothing
    Just name -> case Map.lookup name workspacePackages of
      Nothing -> die
        [ toDoc $ "Selected package " <> show name <> " was not found in the local packages."
        , toDoc "Available packages:"
        , indent (toDoc (Array.fromFoldable $ Map.keys workspacePackages))
        ]
      Just p -> pure (Just p)

  -- Read in the package database
  { compiler: packageSetCompiler, remotePackageSet } <- case workspace.set of
    Nothing -> do
      die $ "Registry solver is not supported yet - please specify a package set"
    Just (SetFromRegistry { registry: v }) -> do
      logDebug "Reading the package set from the Registry repo..."
      let packageSetPath = Path.concat [ Paths.registryPath, "package-sets", Version.printVersion v <> ".json" ]
      liftAff (RegistryJson.readJsonFile packageSetPath) >>= case _ of
        Left err -> die $ "Couldn't read the package set: " <> err
        Right (RemotePackageSet registryPackageSet) -> do
          logInfo "Read the package set from the registry"
          pure
            { compiler: registryPackageSet.compiler
            , remotePackageSet: registryPackageSet.packages
            }
    Just (SetFromUrl { url: rawUrl, hash: maybeHash }) -> do
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
              case RegistryJson.parseJson body of
                Right (RemotePackageSet set) -> do
                  logDebug "Read a new-format package set from URL"
                  pure { compiler: set.compiler, remotePackageSet: set.packages }
                Left err -> do
                  logDebug [ "Couldn't parse remote package set in modern format, error:", "  " <> show err, "Trying with the legacy format..." ]
                  case RegistryJson.parseJson body of
                    Left err' -> die $ "Couldn't parse remote package set, error: " <> show err'
                    Right (Registry.PackageSet.LegacyPackageSet set) -> do
                      logDebug "Read legacy package set from URL"
                      version <- case Map.lookup (unsafeFromRight (PackageName.parse "metadata")) set of
                        Just { version } -> pure (unsafeFromRight (Version.parseVersion Version.Lenient (unwrap version)))
                        Nothing -> die $ "Couldn't find 'metadata' package in legacy package set."
                      pure { compiler: version, remotePackageSet: map RemoteLegacyPackage set }
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
      logDebug $ "Package set hash: " <> show newHash
      liftEffect $ updatePackageSetHashInConfig workspaceDoc newHash
      liftAff $ Yaml.writeYamlDocFile "spago.yaml" workspaceDoc
      pure result

  -- TODO: add a function to make a range from a version and its highest bump
  let
    compatibleCompiler = unsafeFromRight $ Version.parseRange Version.Lenient
      $ ">= "
      <> Version.printVersion packageSetCompiler
      <> " <"
      <> Version.printVersion (Version.bumpHighest packageSetCompiler)

  -- Mix in the package set (a) the workspace packages, and (b) the extra_packages
  -- Note: if there are duplicate packages we pick the "most local ones first",
  -- i.e. first workspace, then extra, then registry packages.
  -- This is to (1) easily allow overriding packages, (2) easily allow "private registries"
  -- and (3) prevent the security hole where people can register new names and take precedence in your build.
  let
    packageSet =
      let
        overrides = Map.union
          (map WorkspacePackage workspacePackages)
          (map fromExtraPackage (fromMaybe Map.empty workspace.extra_packages))

      in
        Map.union
          overrides
          (map fromRemotePackage remotePackageSet)

  case maybeSelected of
    Just selected -> do
      logSuccess $ "Selecting package to build: " <> show selected.package.name
      logDebug $ "Package path: " <> selected.path
    Nothing -> do
      logSuccess
        [ toDoc $ "Selecting " <> show (Map.size workspacePackages) <> " packages to build:"
        , indent2 (toDoc (Set.toUnfoldable $ Map.keys workspacePackages :: Array PackageName))
        ]

  pure
    { selected: maybeSelected
    , packageSet
    , compatibleCompiler
    , backend: workspace.backend
    , output: workspace.output
    , doc: workspaceDoc
    }

getPackageLocation :: PackageName -> Package -> FilePath
getPackageLocation name = case _ of
  RegistryVersion v -> Path.concat [ Paths.localCachePackagesPath, PackageName.print name <> "-" <> Version.printVersion v ]
  GitPackage p -> Path.concat [ Paths.localCachePackagesPath, PackageName.print name, p.ref ]
  LocalPackage p -> p.path
  WorkspacePackage { path } -> path

sourceGlob :: PackageName -> Package -> Array String
sourceGlob name package = map (\p -> Path.concat [ getPackageLocation name package, p ])
  case package of
    WorkspacePackage { hasTests } ->
      if hasTests then
        [ srcGlob, testGlob ]
      else
        [ srcGlob ]
    GitPackage { subdir: Just s } -> [ Path.concat [ s, srcGlob ] ]
    _ -> [ srcGlob ]

srcGlob :: String
srcGlob = "src/**/*.purs"

testGlob :: String
testGlob = "test/**/*.purs"

getWorkspacePackages :: PackageSet -> Array WorkspacePackage
getWorkspacePackages = Array.mapMaybe extractWorkspacePackage <<< Map.toUnfoldable
  where
  extractWorkspacePackage = case _ of
    Tuple _ (WorkspacePackage p) -> Just p
    _ -> Nothing

type PackageSetResult = { compiler :: Version, remotePackageSet :: Map PackageName RemotePackage }

readPackageSetFromHash :: forall a. Sha256 -> Spago (LogEnv a) (Either String PackageSetResult)
readPackageSetFromHash hash = do
  hex <- liftEffect (shaToHex hash)
  let path = packageSetCachePath hex
  logDebug $ "Reading cached package set entry from " <> path
  FS.exists path >>= case _ of
    false -> pure $ Left $ "Did not find a package set cached with hash " <> show hash
    true -> (liftAff $ RegistryJson.readJsonFile path) >>= case _ of
      Left err -> pure $ Left $ "Error while reading cached package set " <> show hash <> ": " <> err
      Right res -> pure $ Right res

writePackageSetToHash :: forall a. PackageSetResult -> Spago (LogEnv a) Sha256
writePackageSetToHash result = do
  let serialised = RegistryJson.printJson result
  hash <- liftEffect $ Registry.Hash.sha256String serialised
  hex <- liftEffect (shaToHex hash)
  FS.mkdirp packageSetsCachePath
  FS.writeTextFile (packageSetCachePath hex) serialised
  pure hash

packageSetsCachePath :: FilePath
packageSetsCachePath = Path.concat [ Paths.globalCachePath, "setsCAS" ]

packageSetCachePath :: HexString → String
packageSetCachePath (HexString hash) = Path.concat [ packageSetsCachePath, hash ]

foreign import updatePackageSetHashInConfigImpl :: EffectFn2 (YamlDoc Config) String Unit

updatePackageSetHashInConfig :: YamlDoc Config -> Sha256 -> Effect Unit
updatePackageSetHashInConfig doc sha = runEffectFn2 updatePackageSetHashInConfigImpl doc (show sha)

foreign import addPackagesToConfigImpl :: EffectFn2 (YamlDoc Config) (Array String) Unit

addPackagesToConfig :: YamlDoc Config -> Array PackageName -> Effect Unit
addPackagesToConfig doc pkgs = runEffectFn2 addPackagesToConfigImpl doc (map PackageName.print pkgs)

toManifest :: Config -> Either String Manifest
toManifest config = do
  package@{ name, description, dependencies: Dependencies deps } <- Either.note "Did not find a package in the config" config.package
  publishConfig <- Either.note "Did not find a `publish` section in the package config" package.publish
  version <- Either.note "Did not find a `version` field in the package config" publishConfig.version
  license <- Either.note "Did not find a `license` field in the package config" publishConfig.license
  location <- Either.note "Did not find a `location` field in the package config" publishConfig.location
  let
    checkRange :: Tuple PackageName (Maybe Range) -> Either String (Tuple PackageName Range)
    checkRange (Tuple packageName maybeRange) = case maybeRange of
      Nothing -> Left $ "Could not get dependency range for package " <> show packageName
      Just r -> Right (Tuple packageName r)
  dependencies <- map Map.fromFoldable $ traverse checkRange (Map.toUnfoldable deps :: Array (Tuple PackageName (Maybe Range)))
  pure $ Manifest
    { version
    , license
    , name
    , location
    , description
    , dependencies
    , owners: Nothing -- TODO specify owners in spago config
    , files: Nothing -- TODO specify files in spago config
    }

findPackageSet :: forall a. Maybe Version -> Spago (PursEnv a) Version
findPackageSet maybeSet = do
  -- first we read in the list of sets
  let
    parseSetVersion str = Version.parseVersion Version.Lenient case String.stripSuffix (Pattern ".json") str of
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
      Nothing -> die $ [ toDoc $ "Could not find desired set " <> show desiredSet <> " in the list of available set versions:" ]
        <> map (indent <<< toDoc <<< show) setVersions
    -- no set in input: read the compiler version, look through the latest set by major version until we match the compiler version
    Nothing -> do
      -- build an index from compiler version to latest set, only looking at major versions
      -- TODO: we should probably cache this, like it's done in the legacy package sets
      let
        readPackageSet setVersion = do
          logDebug "Reading the package set from the Registry repo..."
          let packageSetPath = Path.concat [ packageSetsPath, Version.printVersion setVersion <> ".json" ]
          liftAff (RegistryJson.readJsonFile packageSetPath) >>= case _ of
            Left err -> die $ "Couldn't read the package set: " <> err
            Right (RemotePackageSet registryPackageSet) -> do
              logDebug $ "Read the package set " <> show setVersion <> " from the registry"
              pure registryPackageSet
        accVersions index newSetVersion = do
          -- first thing we check if we already have the latest set for this major version
          let maybeResult = Foldable.find (\(Tuple _c s) -> Version.major s == Version.major newSetVersion) (Map.toUnfoldable index :: Array (Tuple Version Version))
          case maybeResult of
            -- We have a version with the same major. If it's higher then we can just replace it (because we know it supports the same compiler)
            -- If it's not, we just return the current index and move on
            Just (Tuple currentCompiler currentVersion) ->
              if newSetVersion > currentVersion then do
                logDebug $ "Updating to set " <> show newSetVersion <> " for compiler " <> show currentCompiler
                pure (Map.insert currentCompiler newSetVersion index)
              else pure index
            -- Didn't find a version with the same major, so we could be supporting a different compiler here.
            -- Read the set, check what we have for the compiler it supports
            Nothing -> do
              packageSet <- readPackageSet newSetVersion
              logDebug $ "Inserting set " <> show newSetVersion <> " for compiler " <> show packageSet.compiler
              pure $ Map.insert packageSet.compiler newSetVersion index
      index :: Map Version Version <- Array.foldM accVersions Map.empty setVersions
      logDebug $ [ "Package set index", show index ]

      -- now check if the compiler version is in the index
      { purs } <- ask
      case Map.lookup purs.version index of
        Just v -> pure v
        -- TODO: well we could approximate with any minor version really? See old Spago:
        -- https://github.com/purescript/spago/blob/01eecf041851ca0fbced1d4f7147fcbdd8bf168d/src/Spago/PackageSet.hs#L66
        Nothing -> die $ [ toDoc $ "No set is compatible with your compiler version " <> show purs.version, toDoc "Compatible versions:" ]
          <> map (indent <<< toDoc <<< show) (Array.fromFoldable $ Map.keys index)
