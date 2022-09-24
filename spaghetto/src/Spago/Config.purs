module Spago.Config where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core as Core
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either as Either
import Data.HTTP.Method as Method
import Data.Map as Map
import Data.Set as Set
import Dodo as Log
import Foreign.FastGlob as Glob
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.SPDX (License)
import Node.Path as Path
import Parsing as Parsing
import Registry.Hash as Registry.Hash
import Registry.Json as RegistryJson
import Registry.Legacy.PackageSet as Registry.PackageSet
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema as Registry
import Registry.Version (Range, Version)
import Registry.Version as Registry.Version
import Registry.Version as Version
import Spago.Paths as Paths
import Yaml (class ToYaml, (.:), (.:?))
import Yaml as Yaml

type Config =
  { package :: Maybe PackageConfig
  , workspace :: Maybe WorkspaceConfig
  }

type PackageConfig =
  { name :: PackageName
  , version :: Version
  , license :: License
  , dependencies :: Dependencies
  , testDependencies :: Maybe Dependencies
  , bundle :: Maybe BundleConfig
  , publish :: Maybe PublishConfig
  }

type PublishConfig = {} -- FIXME: publishing. Does license go here instead?

type WorkspaceConfig =
  { set :: Maybe SetAddress -- TODO: package set string is optional, if not specified we use the solver
  , extra_packages :: Maybe (Map PackageName GitPackage)
  , backend :: Maybe String -- FIXME support alternate backends
  }

type Workspace =
  { selected :: Maybe WorkspacePackage
  , packageSet :: PackageSet
  , backend :: Maybe String
  }

fromRemotePackage :: RemotePackage -> Package
fromRemotePackage = case _ of
  RemoteGitPackage p -> GitPackage p
  RemoteRegistryVersion v -> RegistryVersion v
  RemoteLegacyPackage e -> GitPackage
    { git: e.repo
    , ref: unwrap e.version
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
      git <- obj RegistryJson..: "git"
      ref <- obj RegistryJson..: "ref"
      pure (RemoteGitPackage { dependencies, git, ref })
    decodeLegacyPkg obj = do
      dependencies <- obj RegistryJson..: "dependencies"
      repo <- obj RegistryJson..: "repo"
      version <- obj RegistryJson..: "version"
      pure (RemoteLegacyPackage { dependencies, repo, version })
    decodeVersion = map RemoteRegistryVersion <<< (RegistryJson.fromEncodableString :: String -> Either String Version)

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

type LocalPackage =
  { path :: FilePath
  , dependencies :: Maybe Dependencies
  }

type GitPackage =
  { git :: String
  , ref :: String
  , dependencies :: Maybe Dependencies -- TODO document that this is possible
  }

newtype Dependencies = Dependencies (Map PackageName (Maybe Range))

derive instance Eq Dependencies

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
  = SetFromRegistry { registry :: String }
  | SetFromUrl { url :: String, hash :: Maybe String }

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
  , entrypoint :: Maybe FilePath
  , outfile :: Maybe FilePath
  , platform :: Maybe Platform
  }

data Platform = PlatformNode | PlatformBrowser

instance Show Platform where
  show = case _ of
    PlatformNode -> "node"
    PlatformBrowser -> "browser"

parsePlatform :: String -> Maybe Platform
parsePlatform = case _ of
  "node" -> Just PlatformNode
  "browser" -> Just PlatformBrowser
  _ -> Nothing

instance ToYaml Platform where
  encode = Core.fromString <<< show
  decode = Either.note "Expected BundlePlatform" <<< parsePlatform <=< Yaml.decode

readConfig :: forall a. FilePath -> Spago (LogEnv a) (Either String Config)
readConfig path = do
  logDebug $ "Reading config from " <> path
  liftAff $ Yaml.readYamlFile path

-- | Reads all the configurations in the tree and builds up the Map of local
-- | packages to be integrated in the package set
readWorkspace :: forall a. Maybe PackageName -> Spago (LogEnv a) Workspace
readWorkspace maybeSelectedPackage = do
  logInfo "Reading Spago workspace configuration..."
  -- First try to read the config in the root. It _has_ to contain a workspace
  -- configuration, or we fail early.
  { workspace, package: maybePackage } <- readConfig "spago.yaml" >>= case _ of
    Left err -> die $ "Couldn't parse Spago config, error:\n  " <> err
    Right { workspace: Nothing } -> die $ "Your spago.yaml doesn't contain a workspace section" -- TODO refer to the docs
    Right { workspace: Just workspace, package } -> pure { workspace, package }

  -- Then gather all the spago other configs in the tree.
  { succeeded: otherConfigPaths, failed } <- liftAff $ Glob.match' Paths.cwd [ "**/spago.yaml" ] { ignore: [ ".spago", "spago.yaml" ] }
  logDebug $ [ toDoc "Found packages at these paths:", Log.indent $ Log.lines (map toDoc otherConfigPaths) ]
  logDebug $ "Failed to sanitise some of the glob matches: " <> show failed

  -- We read all of them in, and only read the package section, if any.
  -- TODO: we should probably not include at all the configs that contain a
  -- workspace configuration, maybe even "prune the tree" whenever we find one
  let
    readWorkspaceConfig path = do
      maybeConfig <- readConfig path
      pure $ case maybeConfig of
        Left e -> Left $ "Could not read config at path " <> path <> "\nError was: " <> e
        Right { package: Nothing } -> Left $ "No package found for config at path: " <> path
        Right { package: Just package } ->
          -- We store the path of the package, so we can treat is basically as a LocalPackage
          Right $ Tuple package.name { path: Path.dirname path, package }
  { right: otherPackages, left: failedPackages } <- partitionMap identity <$> traverse readWorkspaceConfig otherConfigPaths

  -- TODO do we fail here?
  logDebug $ [ "Failed to read some configs:" ] <> failedPackages

  let
    workspacePackages = Map.fromFoldable $ otherPackages <> case maybePackage of
      Nothing -> []
      Just package -> [ Tuple package.name { path: ".", package } ]

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
  -- TODO: actually should the compiler be a maybe??
  -- TODO: stick the compiler in the environment?
  { maybeCompiler, remotePackageSet } <- case workspace.set of
    Nothing -> do
      die $ "Registry solver is not supported yet - please specify a package set"
    Just (SetFromRegistry { registry: v }) -> do
      logDebug "Reading the package set from the Registry repo..."
      let packageSetPath = Path.concat [ Paths.registryPath, "package-sets", v <> ".json" ]
      liftAff (RegistryJson.readJsonFile packageSetPath) >>= case _ of
        Left err -> die $ "Couldn't read the package set: " <> err
        Right (RemotePackageSet registryPackageSet) -> do
          logInfo "Read the package set from the registry"
          pure
            { maybeCompiler: Just registryPackageSet.compiler
            , remotePackageSet: registryPackageSet.packages
            }
    Just (SetFromUrl { url: rawUrl, hash: maybeHash }) -> do
      logDebug $ "Reading the package set from URL: " <> rawUrl
      -- TODO: do something with the hash. If there is a hash then we look up in the CAS, if not we compute a hash and store it there
      -- TODO: use Registry.Hash.sha256String
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
              pure { maybeCompiler: Just set.compiler, remotePackageSet: set.packages }
            Left err -> do
              logDebug [ "Couldn't parse remote package set in modern format, error:", show err, "Trying with the legacy format..." ]
              case RegistryJson.parseJson body of
                Left err' -> die $ "Couldn't parse remote package set, error: " <> show err'
                Right (Registry.PackageSet.LegacyPackageSet set) -> do
                  logDebug "Read legacy package set from URL"
                  -- TODO: fetch compiler version from Metadata package
                  pure { maybeCompiler: Nothing, remotePackageSet: map RemoteLegacyPackage set }

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
          (map GitPackage (fromMaybe Map.empty workspace.extra_packages))

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

  pure { selected: maybeSelected, packageSet, backend: workspace.backend }

getPackageLocation :: PackageName -> Package -> FilePath
getPackageLocation name = case _ of
  RegistryVersion v -> Path.concat [ Paths.localCachePackagesPath, PackageName.print name <> "-" <> Version.printVersion v ]
  GitPackage p -> Path.concat [ Paths.localCachePackagesPath, PackageName.print name, p.ref ]
  LocalPackage p -> p.path
  WorkspacePackage { path } -> path

sourceGlob :: PackageName -> Package -> String
sourceGlob name package = Path.concat [ getPackageLocation name package, "src/**/*.purs" ]

getWorkspacePackages :: PackageSet -> Array WorkspacePackage
getWorkspacePackages = Array.mapMaybe extractWorkspacePackage <<< Map.toUnfoldable
  where
  extractWorkspacePackage = case _ of
    Tuple _ (WorkspacePackage p) -> Just p
    _ -> Nothing