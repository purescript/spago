module Spago.Config
  ( BackendConfig
  , BuildOptionsInput
  , BuildOptions
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
  , RemotePackage
  , RunConfig
  , SetAddress(..)
  , TestConfig
  , WithTestGlobs(..)
  , Workspace
  , WorkspaceConfig
  , WorkspacePackage
  , addPackagesToConfig
  , addRangesToConfig
  , configCodec
  , packageCodec
  , findPackageSet
  , getPackageLocation
  , getWorkspacePackages
  , parseBundleType
  , parsePlatform
  , readConfig
  , readWorkspace
  , sourceGlob
  , gitPackageCodec
  , widestRange
  ) where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Data.Array as Array
import Data.Codec.Argonaut (JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Sum as CA.Sum
import Data.Either as Either
import Data.Foldable as Foldable
import Data.HTTP.Method as Method
import Data.List as List
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Dodo as Log
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object as Foreign
import Node.Path as Path
import Partial.Unsafe (unsafeCrashWith)
import Registry.Foreign.FastGlob as Glob
import Registry.Internal.Codec as Internal.Codec
import Registry.License as License
import Registry.Location as Location
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Spago.FS as FS
import Spago.Git as Git
import Spago.Paths as Paths
import Spago.Purs (PursEnv)

type Config =
  { package :: Maybe PackageConfig
  , workspace :: Maybe WorkspaceConfig
  }

configCodec :: JsonCodec Config
configCodec = CAR.object "Config"
  { package: CAR.optional packageConfigCodec
  , workspace: CAR.optional workspaceConfigCodec
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

packageConfigCodec :: JsonCodec PackageConfig
packageConfigCodec = CAR.object "PackageConfig"
  { name: PackageName.codec
  , description: CAR.optional CA.string
  , dependencies: dependenciesCodec
  , bundle: CAR.optional bundleConfigCodec
  , run: CAR.optional runConfigCodec
  , test: CAR.optional testConfigCodec
  , publish: CAR.optional publishConfigCodec
  }

type PublishConfig =
  { version :: Version
  , license :: License
  , location :: Maybe Location
  }

publishConfigCodec :: JsonCodec PublishConfig
publishConfigCodec = CAR.object "PublishConfig"
  { version: Version.codec
  , license: License.codec
  , location: CAR.optional Location.codec
  }

type RunConfig =
  { main :: Maybe String
  , execArgs :: Maybe (Array String)
  }

runConfigCodec :: JsonCodec RunConfig
runConfigCodec = CAR.object "RunConfig"
  { main: CAR.optional CA.string
  , execArgs: CAR.optional (CA.array CA.string)
  }

type TestConfig =
  { main :: String
  , execArgs :: Maybe (Array String)
  , dependencies :: Dependencies
  }

testConfigCodec :: JsonCodec TestConfig
testConfigCodec = CAR.object "TestConfig"
  { main: CA.string
  , execArgs: CAR.optional (CA.array CA.string)
  , dependencies: dependenciesCodec
  }

type BackendConfig =
  { cmd :: String
  , args :: Maybe (Array String)
  }

backendConfigCodec :: JsonCodec BackendConfig
backendConfigCodec = CAR.object "BackendConfig"
  { cmd: CA.string
  , args: CAR.optional (CA.array CA.string)
  }

type BuildOptionsInput =
  { output :: Maybe FilePath
  , pedantic_packages :: Maybe Boolean
  }

buildOptionsCodec :: JsonCodec BuildOptionsInput
buildOptionsCodec = CAR.object "CompileOptionsInput"
  { output: CAR.optional CA.string
  , pedantic_packages: CAR.optional CA.boolean
  }

type WorkspaceConfig =
  { package_set :: Maybe SetAddress
  , extra_packages :: Maybe (Map PackageName ExtraPackage)
  , backend :: Maybe BackendConfig
  , build_opts :: Maybe BuildOptionsInput
  }

workspaceConfigCodec :: JsonCodec WorkspaceConfig
workspaceConfigCodec = CAR.object "WorkspaceConfig"
  { package_set: CAR.optional setAddressCodec
  , extra_packages: CAR.optional (Internal.Codec.packageMap extraPackageCodec)
  , backend: CAR.optional backendConfigCodec
  , build_opts: CAR.optional buildOptionsCodec
  }

type Workspace =
  { selected :: Maybe WorkspacePackage
  , packageSet :: PackageSet
  , compatibleCompiler :: Range
  , backend :: Maybe BackendConfig
  , buildOptions :: BuildOptions
  , doc :: YamlDoc Config
  }

type BuildOptions =
  { output :: Maybe FilePath
  , pedanticPackages :: Boolean
  }

data ExtraPackage
  = ExtraLocalPackage LocalPackage
  | ExtraRemotePackage RemotePackage

extraPackageCodec :: JsonCodec ExtraPackage
extraPackageCodec = CA.codec' decode encode
  where
  encode (ExtraLocalPackage lp) = CA.encode localPackageCodec lp
  encode (ExtraRemotePackage rp) = CA.encode remotePackageCodec rp

  decode json = map ExtraLocalPackage (CA.decode localPackageCodec json)
    <|> map ExtraRemotePackage (CA.decode remotePackageCodec json)

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
    , ref: e.version
    , subdir: Nothing
    , dependencies: Just $ Dependencies $ Map.fromFoldable $ map (\p -> Tuple p Nothing) e.dependencies
    }

-- | The format of a legacy packages.json package set file
newtype LegacyPackageSet = LegacyPackageSet (Map PackageName LegacyPackageSetEntry)

derive instance Newtype LegacyPackageSet _
derive newtype instance Eq LegacyPackageSet

legacyPackageSetCodec :: JsonCodec LegacyPackageSet
legacyPackageSetCodec =
  Profunctor.wrapIso LegacyPackageSet
    $ Internal.Codec.packageMap legacyPackageSetEntryCodec

legacyPackageSetEntryCodec :: JsonCodec LegacyPackageSetEntry
legacyPackageSetEntryCodec = CAR.object "LegacyPackageSetEntry"
  { dependencies: CA.array PackageName.codec
  , repo: CA.string
  , version: CA.string
  }

-- | The format of a legacy packages.json package set entry for an individual
-- | package.
type LegacyPackageSetEntry =
  { dependencies :: Array PackageName
  , repo :: String
  , version :: String
  }

data RemotePackage
  = RemoteGitPackage GitPackage
  | RemoteRegistryVersion Version
  | RemoteLegacyPackage LegacyPackageSetEntry

derive instance Eq RemotePackage

newtype RemotePackageSet = RemotePackageSet
  { compiler :: Version
  , packages :: Map PackageName RemotePackage
  , version :: Version
  }

remotePackageSetCodec :: JsonCodec RemotePackageSet
remotePackageSetCodec = Profunctor.wrapIso RemotePackageSet $ CAR.object "PackageSet"
  { version: Version.codec
  , compiler: Version.codec
  , packages: Internal.Codec.packageMap remotePackageCodec
  }

remotePackageCodec :: JsonCodec RemotePackage
remotePackageCodec = CA.codec' decode encode
  where
  encode (RemoteRegistryVersion v) = CA.encode Version.codec v
  encode (RemoteGitPackage p) = CA.encode gitPackageCodec p
  encode (RemoteLegacyPackage p) = CA.encode legacyPackageSetEntryCodec p

  decode json = map RemoteRegistryVersion (CA.decode Version.codec json)
    <|> map RemoteGitPackage (CA.decode gitPackageCodec json)
    <|> map RemoteLegacyPackage (CA.decode legacyPackageSetEntryCodec json)

derive instance Newtype RemotePackageSet _
derive newtype instance Eq RemotePackageSet

type PackageSet = Map PackageName Package

type WorkspacePackage =
  { path :: FilePath
  , package :: PackageConfig
  , doc :: YamlDoc Config
  , hasTests :: Boolean
  }

workspacePackageCodec :: JsonCodec WorkspacePackage
workspacePackageCodec = CAR.object "WorkspacePackage"
  { path: CA.string
  , package: packageConfigCodec
  , doc: yamlDocCodec
  , hasTests: CA.boolean
  }

yamlDocCodec :: JsonCodec (YamlDoc Config)
yamlDocCodec = CA.codec' decode encode
  where
  -- TODO: implementation of encode
  encode _x = CA.encode CA.null unit

  -- TODO: implementation of decode if needed
  decode _json = Left MissingValue

data Package
  = RegistryVersion Version
  | GitPackage GitPackage
  | LocalPackage LocalPackage
  | WorkspacePackage WorkspacePackage

packageCodec :: JsonCodec Package
packageCodec = CA.codec' decode encode
  where
  encode (RegistryVersion x) = CA.encode Version.codec x
  encode (GitPackage x) = CA.encode gitPackageCodec x
  encode (LocalPackage x) = CA.encode localPackageCodec x
  encode (WorkspacePackage x) = CA.encode workspacePackageCodec x

  decode json =
    map RegistryVersion (CA.decode Version.codec json)
      <|> map GitPackage (CA.decode gitPackageCodec json)
      <|> map LocalPackage (CA.decode localPackageCodec json)
      <|> map WorkspacePackage (CA.decode workspacePackageCodec json)

type LocalPackage = { path :: FilePath }

localPackageCodec :: JsonCodec LocalPackage
localPackageCodec = CAR.object "LocalPackage" { path: CA.string }

type GitPackage =
  { git :: String
  , ref :: String
  , subdir :: Maybe FilePath -- TODO: document that this is possible
  , dependencies :: Maybe Dependencies -- TODO document that this is possible
  }

gitPackageCodec :: JsonCodec GitPackage
gitPackageCodec = CAR.object "GitPackage"
  { git: CA.string
  , ref: CA.string
  , subdir: CAR.optional CA.string
  , dependencies: CAR.optional dependenciesCodec
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
        Just r1, Just r2 -> Range.intersect r1 r2
    )
    d1
    d2

instance Monoid Dependencies where
  mempty = Dependencies (Map.empty)

dependenciesCodec :: JsonCodec Dependencies
dependenciesCodec = Profunctor.dimap to from $ CA.array dependencyCodec
  where
  packageSingletonCodec = Internal.Codec.packageMap spagoRangeCodec

  to :: Dependencies -> Array (Either PackageName (Map PackageName Range))
  to (Dependencies deps) =
    map
      ( \(Tuple name maybeRange) -> case maybeRange of
          Nothing -> Left name
          Just r -> Right (Map.singleton name r)
      )
      $ Map.toUnfoldable deps :: Array _

  from :: Array (Either PackageName (Map PackageName Range)) -> Dependencies
  from = Dependencies <<< Map.fromFoldable <<< map
    ( case _ of
        Left name -> Tuple name Nothing
        Right m -> rmap Just $ unsafeFromJust (List.head (Map.toUnfoldable m))
    )

  dependencyCodec :: JsonCodec (Either PackageName (Map PackageName Range))
  dependencyCodec = CA.codec' decode encode
    where
    encode = case _ of
      Left name -> CA.encode PackageName.codec name
      Right singletonMap -> CA.encode packageSingletonCodec singletonMap

    decode json =
      map Left (CA.decode PackageName.codec json)
        <|> map Right (CA.decode packageSingletonCodec json)

widestRange :: Range
widestRange = Either.fromRight' (\_ -> unsafeCrashWith "Fake range failed")
  $ Range.parse ">=0.0.0 <2147483647.0.0"

spagoRangeCodec :: JsonCodec Range
spagoRangeCodec = CA.prismaticCodec "SpagoRange" rangeParse printSpagoRange CA.string
  where
  rangeParse str =
    if str == "*" then Just widestRange
    else hush $ Range.parse str

printSpagoRange :: Range -> String
printSpagoRange range =
  if range == widestRange then "*"
  else Range.print range

data SetAddress
  = SetFromRegistry { registry :: Version }
  | SetFromUrl { url :: String, hash :: Maybe Sha256 }

setAddressCodec :: JsonCodec SetAddress
setAddressCodec = CA.codec' decode encode
  where
  setFromRegistryCodec = CAR.object "SetFromRegistry" { registry: Version.codec }
  setFromUrlCodec = CAR.object "SetFromUrl" { url: CA.string, hash: CAR.optional Sha256.codec }

  encode (SetFromRegistry r) = CA.encode setFromRegistryCodec r
  encode (SetFromUrl u) = CA.encode setFromUrlCodec u

  decode json = map SetFromRegistry (CA.decode setFromRegistryCodec json)
    <|> map SetFromUrl (CA.decode setFromUrlCodec json)

type BundleConfig =
  { minify :: Maybe Boolean
  , module :: Maybe String
  , outfile :: Maybe FilePath
  , platform :: Maybe BundlePlatform
  , type :: Maybe BundleType
  }

bundleConfigCodec :: JsonCodec BundleConfig
bundleConfigCodec = CAR.object "BundleConfig"
  { minify: CAR.optional CA.boolean
  , module: CAR.optional CA.string
  , outfile: CAR.optional CA.string
  , platform: CAR.optional bundlePlatformCodec
  , type: CAR.optional bundleTypeCodec
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

bundlePlatformCodec :: JsonCodec BundlePlatform
bundlePlatformCodec = CA.Sum.enumSum show (parsePlatform)

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

bundleTypeCodec :: JsonCodec BundleType
bundleTypeCodec = CA.Sum.enumSum show (parseBundleType)

readConfig :: forall a. FilePath -> Spago (LogEnv a) (Either String { doc :: YamlDoc Config, yaml :: Config })
readConfig path = do
  logDebug $ "Reading config from " <> path
  FS.exists path >>= case _ of
    false -> pure (Left $ "Did not find " <> path <> " file. Run `spago init` to initialise a new project.")
    true -> liftAff $ FS.readYamlDocFile configCodec path

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
  { compiler: packageSetCompiler, remotePackageSet } <- case workspace.package_set of
    Nothing -> do
      die $ "Registry solver is not supported yet - please specify a package set"
    Just (SetFromRegistry { registry: v }) -> do
      logDebug "Reading the package set from the Registry repo..."
      let packageSetPath = Path.concat [ Paths.registryPath, "package-sets", Version.print v <> ".json" ]
      liftAff (FS.readJsonFile remotePackageSetCodec packageSetPath) >>= case _ of
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
      logDebug $ "Package set hash: " <> Sha256.print newHash
      liftEffect $ updatePackageSetHashInConfig workspaceDoc newHash
      liftAff $ FS.writeYamlDocFile "spago.yaml" workspaceDoc
      pure result

  let compatibleCompiler = Range.caret packageSetCompiler

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
      logSuccess $ "Selecting package to build: " <> PackageName.print selected.package.name
      logDebug $ "Package path: " <> selected.path
    Nothing -> do
      logSuccess
        [ toDoc $ "Selecting " <> show (Map.size workspacePackages) <> " packages to build:"
        , indent2 (toDoc (Set.toUnfoldable $ Map.keys workspacePackages :: Array PackageName))
        ]

  let
    (buildOptions :: BuildOptions) =
      { output: _.output =<< workspace.build_opts
      , pedanticPackages: fromMaybe false (_.pedantic_packages =<< workspace.build_opts)
      }

  pure
    { selected: maybeSelected
    , packageSet
    , compatibleCompiler
    , backend: workspace.backend
    , buildOptions
    , doc: workspaceDoc
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
getWorkspacePackages = Array.mapMaybe extractWorkspacePackage <<< Map.toUnfoldable
  where
  extractWorkspacePackage = case _ of
    Tuple _ (WorkspacePackage p) -> Just p
    _ -> Nothing

type PackageSetResult = { compiler :: Version, remotePackageSet :: Map PackageName RemotePackage }

packageSetResultCodec :: JsonCodec PackageSetResult
packageSetResultCodec = CAR.object "PackageSetResult"
  { compiler: Version.codec
  , remotePackageSet: Internal.Codec.packageMap remotePackageCodec
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

foreign import updatePackageSetHashInConfigImpl :: EffectFn2 (YamlDoc Config) String Unit

updatePackageSetHashInConfig :: YamlDoc Config -> Sha256 -> Effect Unit
updatePackageSetHashInConfig doc sha = runEffectFn2 updatePackageSetHashInConfigImpl doc (Sha256.print sha)

foreign import addPackagesToConfigImpl :: EffectFn2 (YamlDoc Config) (Array String) Unit

addPackagesToConfig :: YamlDoc Config -> Array PackageName -> Effect Unit
addPackagesToConfig doc pkgs = runEffectFn2 addPackagesToConfigImpl doc (map PackageName.print pkgs)

foreign import addRangesToConfigImpl :: EffectFn2 (YamlDoc Config) (Foreign.Object String) Unit

addRangesToConfig :: YamlDoc Config -> Map PackageName Range -> Effect Unit
addRangesToConfig doc = runEffectFn2 addRangesToConfigImpl doc
  <<< Foreign.fromFoldable
  <<< map (\(Tuple name range) -> Tuple (PackageName.print name) (printSpagoRange range))
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
        <> map (indent <<< toDoc <<< Version.print) setVersions
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
