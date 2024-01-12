module Spago.Core.Config
  ( BackendConfig
  , WorkspaceBuildOptionsInput
  , CensorBuildWarnings(..)
  , WarningCensorTest(..)
  , StatVerbosity(..)
  , PackageBuildOptionsInput
  , BundleConfig
  , BundlePlatform(..)
  , BundleType(..)
  , Config
  , Dependencies(..)
  , ExtraPackage(..)
  , GitPackage
  , LegacyPackageSetEntry
  , LocalPackage
  , PackageConfig
  , PublishConfig
  , RemotePackage(..)
  , RunConfig
  , SetAddress(..)
  , TestConfig
  , WorkspaceConfig
  , configCodec
  , dependenciesCodec
  , extraPackageCodec
  , gitPackageCodec
  , legacyPackageSetEntryCodec
  , localPackageCodec
  , packageConfigCodec
  , parseBundleType
  , parsePlatform
  , printSpagoRange
  , readConfig
  , remotePackageCodec
  , setAddressCodec
  , widestRange
  ) where

import Spago.Core.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Sum as CA.Sum
import Data.Either as Either
import Data.List as List
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.String (stripSuffix) as String
import Data.String.Pattern (Pattern(..)) as String
import Partial.Unsafe (unsafeCrashWith)
import Registry.Internal.Codec as Internal.Codec
import Registry.License as License
import Registry.Location as Location
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Spago.FS as FS
import Type.Proxy (Proxy(..))

type Config =
  { package :: Maybe PackageConfig
  , workspace :: Maybe WorkspaceConfig
  }

configCodec :: JsonCodec Config
configCodec = CA.object "Config"
  $ CA.recordPropOptional (Proxy :: _ "package") packageConfigCodec
  $ CA.recordPropOptional (Proxy :: _ "workspace") workspaceConfigCodec
  $ CA.record

type PackageConfig =
  { name :: PackageName
  , description :: Maybe String
  , dependencies :: Dependencies
  , build :: Maybe PackageBuildOptionsInput
  , bundle :: Maybe BundleConfig
  , run :: Maybe RunConfig
  , test :: Maybe TestConfig
  , publish :: Maybe PublishConfig
  }

packageConfigCodec :: JsonCodec PackageConfig
packageConfigCodec = CA.object "PackageConfig"
  $ CA.recordProp (Proxy :: _ "name") PackageName.codec
  $ CA.recordPropOptional (Proxy :: _ "description") CA.string
  $ CA.recordProp (Proxy :: _ "dependencies") dependenciesCodec
  $ CA.recordPropOptional (Proxy :: _ "build") packageBuildOptionsCodec
  $ CA.recordPropOptional (Proxy :: _ "bundle") bundleConfigCodec
  $ CA.recordPropOptional (Proxy :: _ "run") runConfigCodec
  $ CA.recordPropOptional (Proxy :: _ "test") testConfigCodec
  $ CA.recordPropOptional (Proxy :: _ "publish") publishConfigCodec
  $ CA.record

type PublishConfig =
  { version :: Version
  , license :: License
  , location :: Maybe Location
  , include :: Maybe (Array FilePath)
  , exclude :: Maybe (Array FilePath)
  }

publishConfigCodec :: JsonCodec PublishConfig
publishConfigCodec = CA.object "PublishConfig"
  $ CA.recordProp (Proxy :: _ "version") Version.codec
  $ CA.recordProp (Proxy :: _ "license") License.codec
  $ CA.recordPropOptional (Proxy :: _ "location") Location.codec
  $ CA.recordPropOptional (Proxy :: _ "include") (CA.array CA.string)
  $ CA.recordPropOptional (Proxy :: _ "exclude") (CA.array CA.string)
  $ CA.record

type RunConfig =
  { main :: Maybe String
  , exec_args :: Maybe (Array String)
  }

runConfigCodec :: JsonCodec RunConfig
runConfigCodec = CA.object "RunConfig"
  $ CA.recordPropOptional (Proxy :: _ "main") CA.string
  $ CA.recordPropOptional (Proxy :: _ "exec_args") (CA.array CA.string)
  $ CA.record

type TestConfig =
  { main :: String
  , exec_args :: Maybe (Array String)
  , dependencies :: Dependencies
  , censor_test_warnings :: Maybe CensorBuildWarnings
  , strict :: Maybe Boolean
  , pedantic_packages :: Maybe Boolean
  }

testConfigCodec :: JsonCodec TestConfig
testConfigCodec = CA.object "TestConfig"
  $ CA.recordProp (Proxy :: _ "main") CA.string
  $ CA.recordPropOptional (Proxy :: _ "exec_args") (CA.array CA.string)
  $ CA.recordPropOptional (Proxy :: _ "censor_test_warnings") censorBuildWarningsCodec
  $ CA.recordPropOptional (Proxy :: _ "strict") CA.boolean
  $ CA.recordPropOptional (Proxy :: _ "pedantic_packages") CA.boolean
  $ CA.recordProp (Proxy :: _ "dependencies") dependenciesCodec
  $ CA.record

type BackendConfig =
  { cmd :: String
  , args :: Maybe (Array String)
  }

backendConfigCodec :: JsonCodec BackendConfig
backendConfigCodec = CA.object "BackendConfig"
  $ CA.recordProp (Proxy :: _ "cmd") CA.string
  $ CA.recordPropOptional (Proxy :: _ "args") (CA.array CA.string)
  $ CA.record

type PackageBuildOptionsInput =
  { censor_project_warnings :: Maybe CensorBuildWarnings
  , strict :: Maybe Boolean
  , pedantic_packages :: Maybe Boolean
  }

packageBuildOptionsCodec :: JsonCodec PackageBuildOptionsInput
packageBuildOptionsCodec = CA.object "PackageBuildOptionsInput"
  $ CA.recordPropOptional (Proxy :: _ "censor_project_warnings") censorBuildWarningsCodec
  $ CA.recordPropOptional (Proxy :: _ "strict") CA.boolean
  $ CA.recordPropOptional (Proxy :: _ "pedantic_packages") CA.boolean
  $ CA.record

type BundleConfig =
  { minify :: Maybe Boolean
  , module :: Maybe String
  , outfile :: Maybe FilePath
  , platform :: Maybe BundlePlatform
  , type :: Maybe BundleType
  , extra_args :: Maybe (Array String)
  }

bundleConfigCodec :: JsonCodec BundleConfig
bundleConfigCodec = CA.object "BundleConfig"
  $ CA.recordPropOptional (Proxy :: _ "minify") CA.boolean
  $ CA.recordPropOptional (Proxy :: _ "module") CA.string
  $ CA.recordPropOptional (Proxy :: _ "outfile") CA.string
  $ CA.recordPropOptional (Proxy :: _ "platform") bundlePlatformCodec
  $ CA.recordPropOptional (Proxy :: _ "type") bundleTypeCodec
  $ CA.recordPropOptional (Proxy :: _ "extra_args") (CA.array CA.string)
  $ CA.record

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

type WorkspaceConfig =
  { package_set :: Maybe SetAddress
  , extra_packages :: Maybe (Map PackageName ExtraPackage)
  , backend :: Maybe BackendConfig
  , build_opts :: Maybe WorkspaceBuildOptionsInput
  }

workspaceConfigCodec :: JsonCodec WorkspaceConfig
workspaceConfigCodec = CA.object "WorkspaceConfig"
  $ CA.recordPropOptional (Proxy :: _ "package_set") setAddressCodec
  $ CA.recordPropOptional (Proxy :: _ "backend") backendConfigCodec
  $ CA.recordPropOptional (Proxy :: _ "build_opts") buildOptionsCodec
  $ CA.recordPropOptional (Proxy :: _ "extra_packages") (Internal.Codec.packageMap extraPackageCodec)
  $ CA.record

type WorkspaceBuildOptionsInput =
  { output :: Maybe FilePath
  , censor_library_warnings :: Maybe CensorBuildWarnings
  , stat_verbosity :: Maybe StatVerbosity
  }

buildOptionsCodec :: JsonCodec WorkspaceBuildOptionsInput
buildOptionsCodec = CA.object "WorkspaceBuildOptionsInput"
  $ CA.recordPropOptional (Proxy :: _ "output") CA.string
  $ CA.recordPropOptional (Proxy :: _ "censor_library_warnings") censorBuildWarningsCodec
  $ CA.recordPropOptional (Proxy :: _ "stat_verbosity") statVerbosityCodec
  $ CA.record

data CensorBuildWarnings
  = CensorAllWarnings
  | CensorSpecificWarnings (NonEmptyArray WarningCensorTest)

derive instance Eq CensorBuildWarnings

instance Show CensorBuildWarnings where
  show = case _ of
    CensorAllWarnings -> "CensorAllWarnings"
    CensorSpecificWarnings censorTests -> "(CensorSpecificWarnings " <> show censorTests <> ")"

censorBuildWarningsCodec :: JsonCodec CensorBuildWarnings
censorBuildWarningsCodec = CA.codec' parse print
  where
  print = case _ of
    CensorAllWarnings -> CA.encode CA.string "all"
    CensorSpecificWarnings censorTests -> CA.encode (CA.array warningCensorTestCodec) $ NonEmptyArray.toArray censorTests

  parse j = decodeNoneOrAll <|> decodeSpecific
    where
    decodeNoneOrAll = CA.decode CA.string j >>= case _ of
      "all" -> Right CensorAllWarnings
      _ -> Left $ CA.UnexpectedValue j

    decodeSpecific = CensorSpecificWarnings <$> do
      arr <- CA.decode (CA.array warningCensorTestCodec) j
      Either.note (CA.UnexpectedValue j) $ NonEmptyArray.fromArray arr

data WarningCensorTest
  = ByCode String
  | ByMessagePrefix String

derive instance Eq WarningCensorTest
instance Show WarningCensorTest where
  show = case _ of
    ByCode str -> "(ByCode" <> str <> ")"
    ByMessagePrefix str -> "(ByMessagePrefix" <> str <> ")"

warningCensorTestCodec :: JsonCodec WarningCensorTest
warningCensorTestCodec = CA.codec' parse print
  where
  print = case _ of
    ByCode str -> CA.encode CA.string str
    ByMessagePrefix str -> CA.encode byMessagePrefixCodec { by_prefix: str }

  parse j = byCode <|> byPrefix
    where
    byCode = ByCode <$> CA.decode CA.string j
    byPrefix = (ByMessagePrefix <<< _.by_prefix) <$> CA.decode byMessagePrefixCodec j

  byMessagePrefixCodec = CAR.object "ByMessagePrefix" { by_prefix: CA.string }

data StatVerbosity
  = NoStats
  | CompactStats
  | VerboseStats

instance Show StatVerbosity where
  show = case _ of
    NoStats -> "NoStats"
    CompactStats -> "CompactStats"
    VerboseStats -> "VerboseStats"

statVerbosityCodec :: JsonCodec StatVerbosity
statVerbosityCodec = CA.Sum.enumSum print parse
  where
  print = case _ of
    NoStats -> "no-stats"
    CompactStats -> "compact-stats"
    VerboseStats -> "verbose-stats"
  parse = case _ of
    "no-stats" -> Just NoStats
    "compact-stats" -> Just CompactStats
    "verbose-stats" -> Just VerboseStats
    _ -> Nothing

data SetAddress
  = SetFromRegistry { registry :: Version }
  | SetFromUrl { url :: String, hash :: Maybe Sha256 }
  | SetFromPath { path :: FilePath }

derive instance Eq SetAddress

setAddressCodec :: JsonCodec SetAddress
setAddressCodec = CA.codec' decode encode
  where
  setFromRegistryCodec = CAR.object "SetFromRegistry" { registry: Version.codec }
  setFromUrlCodec = CAR.object "SetFromUrl" { url: CA.string, hash: CAR.optional Sha256.codec }
  setFromPathCodec = CAR.object "SetFromPath" { path: CA.string }

  encode (SetFromRegistry r) = CA.encode setFromRegistryCodec r
  encode (SetFromUrl u) = CA.encode setFromUrlCodec u
  encode (SetFromPath p) = CA.encode setFromPathCodec p

  decode json = map SetFromRegistry (CA.decode setFromRegistryCodec json)
    <|> map SetFromUrl (CA.decode setFromUrlCodec json)
    <|> map SetFromPath (CA.decode setFromPathCodec json)

data ExtraPackage
  = ExtraLocalPackage LocalPackage
  | ExtraRemotePackage RemotePackage

derive instance Eq ExtraPackage

extraPackageCodec :: JsonCodec ExtraPackage
extraPackageCodec = CA.codec' decode encode
  where
  encode (ExtraLocalPackage lp) = CA.encode localPackageCodec lp
  encode (ExtraRemotePackage rp) = CA.encode remotePackageCodec rp

  decode json = map ExtraLocalPackage (CA.decode localPackageCodec json)
    <|> map ExtraRemotePackage (CA.decode remotePackageCodec json)

type LocalPackage = { path :: FilePath }

localPackageCodec :: JsonCodec LocalPackage
localPackageCodec = CAR.object "LocalPackage" { path: CA.string }

data RemotePackage
  = RemoteGitPackage GitPackage
  | RemoteRegistryVersion Version
  | RemoteLegacyPackage LegacyPackageSetEntry

derive instance Eq RemotePackage

remotePackageCodec :: JsonCodec RemotePackage
remotePackageCodec = CA.codec' decode encode
  where
  encode (RemoteRegistryVersion v) = CA.encode Version.codec v
  encode (RemoteGitPackage p) = CA.encode gitPackageCodec p
  encode (RemoteLegacyPackage p) = CA.encode legacyPackageSetEntryCodec p

  decode json = map RemoteRegistryVersion (CA.decode Version.codec json)
    <|> map RemoteGitPackage (CA.decode gitPackageCodec json)
    <|> map RemoteLegacyPackage (CA.decode legacyPackageSetEntryCodec json)

type GitPackage =
  { git :: String
  , ref :: String
  , subdir :: Maybe FilePath
  , dependencies :: Maybe Dependencies
  }

gitPackageCodec :: JsonCodec GitPackage
gitPackageCodec = CA.object "GitPackage"
  $ CA.recordProp (Proxy :: _ "git") CA.string
  $ CA.recordProp (Proxy :: _ "ref") CA.string
  $ CA.recordPropOptional (Proxy :: _ "subdir") CA.string
  $ CA.recordPropOptional (Proxy :: _ "dependencies") dependenciesCodec
  $ CA.record

-- | The format of a legacy packages.json package set entry for an individual
-- | package.
type LegacyPackageSetEntry =
  { dependencies :: Array PackageName
  , repo :: String
  , version :: String
  }

legacyPackageSetEntryCodec :: JsonCodec LegacyPackageSetEntry
legacyPackageSetEntryCodec = CA.object "LegacyPackageSetEntry"
  $ CA.recordProp (Proxy :: _ "repo") CA.string
  $ CA.recordProp (Proxy :: _ "version") CA.string
  $ CA.recordProp (Proxy :: _ "dependencies") (CA.array PackageName.codec)
  $ CA.record

readConfig :: forall a. FilePath -> Spago (LogEnv a) (Either (Array String) { doc :: YamlDoc Config, yaml :: Config })
readConfig path = do
  logDebug $ "Reading config from " <> path
  FS.exists path >>= case _ of
    false -> do
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
          [ "Did not find `" <> path <> "`. Spago's configuration files must end with `.yaml`, not `.yml`. "
          , "Try renaming `" <> y  <> "` to `" <> path <> "` or run `spago init` to initialize a new project."
          ]
        _, Nothing ->
          ["Did not find `" <> path <> "`."]
        _, Just y -> 
            [ "Did not find `" <> path <> "`. Spago's configuration files must end with `.yaml`, not `.yml`. "
            , "Try renaming `" <> y  <> "` to `" <> path <> "`."
            ]
    true -> liftAff $ map (lmap pure) $ FS.readYamlDocFile configCodec path
