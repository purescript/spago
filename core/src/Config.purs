module Spago.Core.Config
  ( BackendConfig
  , BundleConfig
  , BundlePlatform(..)
  , BundleType(..)
  , CensorBuildWarnings(..)
  , Config
  , Dependencies(..)
  , ExtraPackage(..)
  , GitPackage
  , LegacyPackageSetEntry
  , LocalPackage
  , PackageBuildOptionsInput
  , PackageConfig
  , PublishConfig
  , RemotePackage(..)
  , RunConfig
  , SetAddress(..)
  , StatVerbosity(..)
  , TestConfig
  , WarningCensorTest(..)
  , WorkspaceBuildOptionsInput
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
  , remotePackageCodec
  , setAddressCodec
  , widestRange
  ) where

import Spago.Core.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Codec.JSON.Sum as CJ.Sum
import Data.Either as Either
import Data.List as List
import Data.Map as Map
import Data.Profunctor as Profunctor
import Partial.Unsafe (unsafeCrashWith)
import Registry.Internal.Codec as Internal.Codec
import Registry.License as License
import Registry.Location as Location
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Type.Proxy (Proxy(..))

type Config =
  { package :: Maybe PackageConfig
  , workspace :: Maybe WorkspaceConfig
  }

configCodec :: CJ.Codec Config
configCodec = CJ.object
  $ CJ.recordPropOptional (Proxy @"package") packageConfigCodec
  $ CJ.recordPropOptional (Proxy @"workspace") workspaceConfigCodec
  $ CJ.record

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

packageConfigCodec :: CJ.Codec PackageConfig
packageConfigCodec = CJ.named "PackageConfig" $ CJ.object
  $ CJ.recordProp (Proxy @"name") PackageName.codec
  $ CJ.recordPropOptional (Proxy @"description") CJ.string
  $ CJ.recordProp (Proxy @"dependencies") dependenciesCodec
  $ CJ.recordPropOptional (Proxy @"build") packageBuildOptionsCodec
  $ CJ.recordPropOptional (Proxy @"bundle") bundleConfigCodec
  $ CJ.recordPropOptional (Proxy @"run") runConfigCodec
  $ CJ.recordPropOptional (Proxy @"test") testConfigCodec
  $ CJ.recordPropOptional (Proxy @"publish") publishConfigCodec
  $ CJ.record

type PublishConfig =
  { version :: Version
  , license :: License
  , location :: Maybe Location
  , include :: Maybe (Array FilePath)
  , exclude :: Maybe (Array FilePath)
  }

publishConfigCodec :: CJ.Codec PublishConfig
publishConfigCodec = CJ.named "PublishConfig" $ CJ.object
  $ CJ.recordProp (Proxy @"version") Version.codec
  $ CJ.recordProp (Proxy @"license") License.codec
  $ CJ.recordPropOptional (Proxy @"location") Location.codec
  $ CJ.recordPropOptional (Proxy @"include") (CJ.array CJ.string)
  $ CJ.recordPropOptional (Proxy @"exclude") (CJ.array CJ.string)
  $ CJ.record

type RunConfig =
  { main :: Maybe String
  , execArgs :: Maybe (Array String)
  }

runConfigCodec :: CJ.Codec RunConfig
runConfigCodec = CJ.named "RunConfig" $ CJ.object
  $ CJ.recordPropOptional (Proxy @"main") CJ.string
  $ CJ.recordPropOptional (Proxy @"execArgs") (CJ.array CJ.string)
  $ CJ.record

type TestConfig =
  { main :: String
  , execArgs :: Maybe (Array String)
  , dependencies :: Dependencies
  , censorTestWarnings :: Maybe CensorBuildWarnings
  , strict :: Maybe Boolean
  , pedanticPackages :: Maybe Boolean
  }

testConfigCodec :: CJ.Codec TestConfig
testConfigCodec = CJ.named "TestConfig" $ CJ.object
  $ CJ.recordProp (Proxy @"main") CJ.string
  $ CJ.recordPropOptional (Proxy @"execArgs") (CJ.array CJ.string)
  $ CJ.recordPropOptional (Proxy @"censorTestWarnings") censorBuildWarningsCodec
  $ CJ.recordPropOptional (Proxy @"strict") CJ.boolean
  $ CJ.recordPropOptional (Proxy @"pedanticPackages") CJ.boolean
  $ CJ.recordProp (Proxy @"dependencies") dependenciesCodec
  $ CJ.record

type BackendConfig =
  { cmd :: String
  , args :: Maybe (Array String)
  }

backendConfigCodec :: CJ.Codec BackendConfig
backendConfigCodec = CJ.named "BackendConfig" $ CJ.object
  $ CJ.recordProp (Proxy @"cmd") CJ.string
  $ CJ.recordPropOptional (Proxy @"args") (CJ.array CJ.string)
  $ CJ.record

type PackageBuildOptionsInput =
  { censorProjectWarnings :: Maybe CensorBuildWarnings
  , strict :: Maybe Boolean
  , pedanticPackages :: Maybe Boolean
  }

packageBuildOptionsCodec :: CJ.Codec PackageBuildOptionsInput
packageBuildOptionsCodec = CJ.named "PackageBuildOptionsInput" $ CJ.object
  $ CJ.recordPropOptional (Proxy @"censorProjectWarnings") censorBuildWarningsCodec
  $ CJ.recordPropOptional (Proxy @"strict") CJ.boolean
  $ CJ.recordPropOptional (Proxy @"pedanticPackages") CJ.boolean
  $ CJ.record

type BundleConfig =
  { minify :: Maybe Boolean
  , module :: Maybe String
  , outfile :: Maybe FilePath
  , platform :: Maybe BundlePlatform
  , type :: Maybe BundleType
  , extraArgs :: Maybe (Array String)
  }

bundleConfigCodec :: CJ.Codec BundleConfig
bundleConfigCodec = CJ.named "BundleConfig" $ CJ.object
  $ CJ.recordPropOptional (Proxy @"minify") CJ.boolean
  $ CJ.recordPropOptional (Proxy @"module") CJ.string
  $ CJ.recordPropOptional (Proxy @"outfile") CJ.string
  $ CJ.recordPropOptional (Proxy @"platform") bundlePlatformCodec
  $ CJ.recordPropOptional (Proxy @"type") bundleTypeCodec
  $ CJ.recordPropOptional (Proxy @"extraArgs") (CJ.array CJ.string)
  $ CJ.record

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

bundlePlatformCodec :: CJ.Codec BundlePlatform
bundlePlatformCodec = CJ.Sum.enumSum show (parsePlatform)

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

bundleTypeCodec :: CJ.Codec BundleType
bundleTypeCodec = CJ.Sum.enumSum show (parseBundleType)

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

dependenciesCodec :: CJ.Codec Dependencies
dependenciesCodec = Profunctor.dimap to from $ CJ.array dependencyCodec
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

  dependencyCodec :: CJ.Codec (Either PackageName (Map PackageName Range))
  dependencyCodec = Codec.codec' decode encode
    where
    encode = case _ of
      Left name -> CJ.encode PackageName.codec name
      Right singletonMap -> CJ.encode packageSingletonCodec singletonMap

    decode json =
      map Left (Codec.decode PackageName.codec json)
        <|> map Right (Codec.decode packageSingletonCodec json)

widestRange :: Range
widestRange = Either.fromRight' (\_ -> unsafeCrashWith "Fake range failed")
  $ Range.parse ">=0.0.0 <2147483647.0.0"

spagoRangeCodec :: CJ.Codec Range
spagoRangeCodec = CJ.prismaticCodec "SpagoRange" rangeParse printSpagoRange CJ.string
  where
  rangeParse str =
    if str == "*" then Just widestRange
    else hush $ Range.parse str

printSpagoRange :: Range -> String
printSpagoRange range =
  if range == widestRange then "*"
  else Range.print range

type WorkspaceConfig =
  { packageSet :: Maybe SetAddress
  , extraPackages :: Maybe (Map PackageName ExtraPackage)
  , backend :: Maybe BackendConfig
  , buildOpts :: Maybe WorkspaceBuildOptionsInput
  }

workspaceConfigCodec :: CJ.Codec WorkspaceConfig
workspaceConfigCodec = CJ.named "WorkspaceConfig" $ CJ.object
  $ CJ.recordPropOptional (Proxy @"packageSet") setAddressCodec
  $ CJ.recordPropOptional (Proxy @"backend") backendConfigCodec
  $ CJ.recordPropOptional (Proxy @"buildOpts") buildOptionsCodec
  $ CJ.recordPropOptional (Proxy @"extraPackages") (Internal.Codec.packageMap extraPackageCodec)
  $ CJ.record

type WorkspaceBuildOptionsInput =
  { output :: Maybe FilePath
  , censorLibraryWarnings :: Maybe CensorBuildWarnings
  , statVerbosity :: Maybe StatVerbosity
  }

buildOptionsCodec :: CJ.Codec WorkspaceBuildOptionsInput
buildOptionsCodec = CJ.named "WorkspaceBuildOptionsInput" $ CJ.object
  $ CJ.recordPropOptional (Proxy @"output") CJ.string
  $ CJ.recordPropOptional (Proxy @"censorLibraryWarnings") censorBuildWarningsCodec
  $ CJ.recordPropOptional (Proxy @"statVerbosity") statVerbosityCodec
  $ CJ.record

data CensorBuildWarnings
  = CensorAllWarnings
  | CensorSpecificWarnings (NonEmptyArray WarningCensorTest)

derive instance Eq CensorBuildWarnings

instance Show CensorBuildWarnings where
  show = case _ of
    CensorAllWarnings -> "CensorAllWarnings"
    CensorSpecificWarnings censorTests -> "(CensorSpecificWarnings " <> show censorTests <> ")"

censorBuildWarningsCodec :: CJ.Codec CensorBuildWarnings
censorBuildWarningsCodec = Codec.codec' decode encode
  where
  encode = case _ of
    CensorAllWarnings -> CJ.encode CJ.string "all"
    CensorSpecificWarnings censorTests -> CJ.encode (CJ.array warningCensorTestCodec) $ NonEmptyArray.toArray censorTests

  decode json = decodeNoneOrAll <|> decodeSpecific
    where
    decodeNoneOrAll = except $ CJ.decode CJ.string json >>= case _ of
      "all" -> Right CensorAllWarnings
      _ -> Left $ CJ.DecodeError.basic "Expected 'all' for all warnings"

    decodeSpecific = CensorSpecificWarnings <$> do
      arr <- Codec.decode (CJ.array warningCensorTestCodec) json
      except $ Either.note (CJ.DecodeError.basic "Expected array of warning codes") $ NonEmptyArray.fromArray arr

data WarningCensorTest
  = ByCode String
  | ByMessagePrefix String

derive instance Eq WarningCensorTest
instance Show WarningCensorTest where
  show = case _ of
    ByCode str -> "(ByCode" <> str <> ")"
    ByMessagePrefix str -> "(ByMessagePrefix" <> str <> ")"

warningCensorTestCodec :: CJ.Codec WarningCensorTest
warningCensorTestCodec = Codec.codec' decode encode
  where
  encode = case _ of
    ByCode str -> CJ.encode CJ.string str
    ByMessagePrefix str -> CJ.encode byMessagePrefixCodec { byPrefix: str }

  decode json = byCode <|> byPrefix
    where
    byCode = ByCode <$> Codec.decode CJ.string json
    byPrefix = (ByMessagePrefix <<< _.byPrefix) <$> Codec.decode byMessagePrefixCodec json

  byMessagePrefixCodec = CJ.named "ByMessagePrefix" $ CJ.Record.object { byPrefix: CJ.string }

data StatVerbosity
  = NoStats
  | CompactStats
  | VerboseStats

instance Show StatVerbosity where
  show = case _ of
    NoStats -> "NoStats"
    CompactStats -> "CompactStats"
    VerboseStats -> "VerboseStats"

statVerbosityCodec :: CJ.Codec StatVerbosity
statVerbosityCodec = CJ.Sum.enumSum print parse
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

setAddressCodec :: CJ.Codec SetAddress
setAddressCodec = Codec.codec' decode encode
  where
  setFromRegistryCodec = CJ.named "SetFromRegistry" $ CJ.Record.object { registry: Version.codec }
  setFromUrlCodec = CJ.named "SetFromUrl" $ CJ.Record.object { url: CJ.string, hash: CJ.Record.optional Sha256.codec }
  setFromPathCodec = CJ.named "SetFromPath" $ CJ.Record.object { path: CJ.string }

  encode (SetFromRegistry r) = CJ.encode setFromRegistryCodec r
  encode (SetFromUrl u) = CJ.encode setFromUrlCodec u
  encode (SetFromPath p) = CJ.encode setFromPathCodec p

  decode json = map SetFromRegistry (Codec.decode setFromRegistryCodec json)
    <|> map SetFromUrl (Codec.decode setFromUrlCodec json)
    <|> map SetFromPath (Codec.decode setFromPathCodec json)

data ExtraPackage
  = ExtraLocalPackage LocalPackage
  | ExtraRemotePackage RemotePackage

derive instance Eq ExtraPackage

extraPackageCodec :: CJ.Codec ExtraPackage
extraPackageCodec = Codec.codec' decode encode
  where
  encode (ExtraLocalPackage lp) = CJ.encode localPackageCodec lp
  encode (ExtraRemotePackage rp) = CJ.encode remotePackageCodec rp

  decode json = map ExtraLocalPackage (Codec.decode localPackageCodec json)
    <|> map ExtraRemotePackage (Codec.decode remotePackageCodec json)

type LocalPackage = { path :: FilePath }

localPackageCodec :: CJ.Codec LocalPackage
localPackageCodec = CJ.named "LocalPackage" $ CJ.Record.object { path: CJ.string }

data RemotePackage
  = RemoteGitPackage GitPackage
  | RemoteRegistryVersion Version
  | RemoteLegacyPackage LegacyPackageSetEntry

derive instance Eq RemotePackage

remotePackageCodec :: CJ.Codec RemotePackage
remotePackageCodec = Codec.codec' decode encode
  where
  encode (RemoteRegistryVersion v) = CJ.encode Version.codec v
  encode (RemoteGitPackage p) = CJ.encode gitPackageCodec p
  encode (RemoteLegacyPackage p) = CJ.encode legacyPackageSetEntryCodec p

  decode json = map RemoteRegistryVersion (Codec.decode Version.codec json)
    <|> map RemoteGitPackage (Codec.decode gitPackageCodec json)
    <|> map RemoteLegacyPackage (Codec.decode legacyPackageSetEntryCodec json)

type GitPackage =
  { git :: String
  , ref :: String
  , subdir :: Maybe FilePath
  , dependencies :: Maybe Dependencies
  }

gitPackageCodec :: CJ.Codec GitPackage
gitPackageCodec = CJ.named "GitPackage" $ CJ.object
  $ CJ.recordProp (Proxy @"git") CJ.string
  $ CJ.recordProp (Proxy @"ref") CJ.string
  $ CJ.recordPropOptional (Proxy @"subdir") CJ.string
  $ CJ.recordPropOptional (Proxy @"dependencies") dependenciesCodec
  $ CJ.record

-- | The format of a legacy packages.json package set entry for an individual
-- | package.
type LegacyPackageSetEntry =
  { dependencies :: Array PackageName
  , repo :: String
  , version :: String
  }

legacyPackageSetEntryCodec :: CJ.Codec LegacyPackageSetEntry
legacyPackageSetEntryCodec = CJ.named "LegacyPackageSetEntry" $ CJ.object
  $ CJ.recordProp (Proxy @"repo") CJ.string
  $ CJ.recordProp (Proxy @"version") CJ.string
  $ CJ.recordProp (Proxy @"dependencies") (CJ.array PackageName.codec)
  $ CJ.record
