module Spago.Config where

import Spago.Prelude

import Data.Argonaut.Core as Core
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either as Either
import Data.Map as Map
import Data.String (Pattern(..))
import Data.String as String
import Foreign.FastGlob as Glob
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.SPDX (License)
import Node.Path as Path
import Parsing as Parsing
import Registry.Json as RegistryJson
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema as Registry
import Registry.Version (Range, Version)
import Registry.Version as Registry.Version
import Registry.Version as Version
import Spago.Paths as Paths
import Yaml (class ToYaml)
import Yaml as Yaml

type Config =
  { package :: Maybe PackageConfig
  , workspace :: Maybe WorkspaceConfig
  }

type PackageConfig =
  { name :: PackageName
  , license :: License
  , dependencies :: Dependencies
  , testDependencies :: Maybe Dependencies
  , bundle :: Maybe BundleConfig
  , publish :: Maybe PublishConfig
  }

type PublishConfig = {} -- FIXME: publishing. Does license go here instead?

type WorkspaceConfig =
  { set :: Maybe String -- TODO: package set string is optional, if not specified we use the solver
  , extra_packages :: Maybe (Map PackageName GitPackage)
  , backend :: Maybe String -- FIXME support alternate backends
  }

type Workspace =
  { selected :: WorkspacePackage
  , packageSet :: PackageSet
  , backend :: Maybe String
  }

type PackageSet = Map PackageName Package

type WorkspacePackage =
  { path :: FilePath
  , package :: PackageConfig
  }

data Package
  = RegistryVersion Version
  | RemoteGitPackage GitPackage
  | LocalPackage LocalPackage
  | WorkspacePackage WorkspacePackage

instance Show Package where
  show = case _ of
    RegistryVersion v -> show v
    RemoteGitPackage p -> show p
    _ -> "TODO" -- FIXME this show instance

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

instance Show Dependencies where
  show (Dependencies ds) = show ds

instance Yaml.ToYaml Dependencies where
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
  logDebug "Reading config.."
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
        Right { package: Just package } -> Right $ Tuple package.name { path, package }
  { right: otherPackages, left: failedPackages } <- partitionMap identity <$> traverse readWorkspaceConfig otherConfigPaths

  -- TODO do we fail here?
  logDebug $ [ "Failed to read some configs:" ] <> failedPackages

  let
    workspacePackages = Map.fromFoldable $ otherPackages <> case maybePackage of
      Nothing -> []
      Just package -> [ Tuple package.name { path: "spago.yaml", package } ]

  -- Select the package for spago to handle during the rest of the execution
  selected <- case maybeSelectedPackage, maybePackage of
    Nothing, Nothing -> case Array.uncons (Map.toUnfoldable workspacePackages) of
      Nothing -> die "No valid packages found in the current project, halting."
      -- If there's only one package and it's not in the root we still select that
      Just { head: (Tuple packageName package), tail: [] } -> do
        logDebug $ "Selecting package " <> show packageName <> " from " <> package.path
        pure package
      -- TODO: well, in some cases we'd like to build all the packages in the workspace together, so maybe we should have some AllPackages selection?
      _ -> die [ "No local package was selected for the build, halting.", "Available packages: " <> show (Map.keys workspacePackages) ]
    Nothing, Just package -> pure { path: "spago.yaml", package }
    Just name, _ -> case Map.lookup name workspacePackages of
      Nothing -> die [ "Selected package " <> show name <> " was not found in the local packages.", "Available packages: " <> show workspacePackages ]
      Just p -> pure p

  -- Read in the package database
  packageSet <- case workspace.set of
    Nothing -> do
      die $ "Registry solver is not supported yet - please specify a package set"
    Just set -> do
      logDebug "Reading the package set"
      -- TODO: try to parse the set field, it might be a URL instead of a version number
      -- FIXME: this allows us to support old-style package sets too
      let packageSetPath = Path.concat [ Paths.registryPath, "package-sets", set <> ".json" ]
      liftAff (RegistryJson.readJsonFile packageSetPath) >>= case _ of
        Left err -> die $ "Couldn't read the package set: " <> err
        Right (Registry.PackageSet registryPackageSet) -> do
          logInfo "Read the package set from the registry"

          -- Mix in the package set (a) the workspace packages, and (b) the extra_packages
          -- Note: if there are duplicate packages we pick the "most local ones first",
          -- i.e. first workspace, then extra, then registry packages.
          -- This is to (1) easily allow overriding packages, (2) easily allow "private registries"
          -- and (3) prevent the security hole where people can register new names and take precedence in your build.
          let
            overrides = Map.union
              (map WorkspacePackage workspacePackages)
              (map RemoteGitPackage (fromMaybe Map.empty workspace.extra_packages))

          pure $ Map.union
            overrides
            (map RegistryVersion registryPackageSet.packages)

  logSuccess $ "Selecting package " <> show selected

  pure { selected, packageSet, backend: workspace.backend }

getPackageLocation :: PackageName -> Package -> FilePath
getPackageLocation name = case _ of
  RegistryVersion v -> Path.concat [ Paths.localCachePackagesPath, PackageName.print name <> "-" <> Version.printVersion v ]
  RemoteGitPackage p -> Path.concat [ Paths.localCachePackagesPath, PackageName.print name, p.ref ]
  LocalPackage p -> p.path
  WorkspacePackage { path } -> case String.stripSuffix (Pattern "spago.yaml") path of
    Nothing -> path
    Just p -> p

sourceGlob :: PackageName -> Package -> String
sourceGlob name package = Path.concat [ getPackageLocation name package, "src/**/*.purs" ]
