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
  , publishLocationCodec
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
import Data.Codec.JSON.Strict as CJS
import Data.Codec.JSON.Sum as CJ.Sum
import Data.Either as Either
import Data.List as List
import Data.Map as Map
import Data.Profunctor as Profunctor
import Partial.Unsafe (unsafeCrashWith)
import Registry.Internal.Codec as Reg.Internal.Codec
import Registry.Internal.Parsing as Reg.Internal.Parsing
import Registry.License as License
import Registry.Location as Location
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version

type Config =
  { package :: Maybe PackageConfig
  , workspace :: Maybe WorkspaceConfig
  }

configCodec :: CJ.Codec Config
configCodec = CJS.objectStrict
  $ CJS.recordPropOptional @"package" packageConfigCodec
  $ CJS.recordPropOptional @"workspace" workspaceConfigCodec
  $ CJS.record

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
packageConfigCodec = CJ.named "PackageConfig" $ CJS.objectStrict
  $ CJS.recordProp @"name" PackageName.codec
  $ CJS.recordPropOptional @"description" CJ.string
  $ CJS.recordProp @"dependencies" dependenciesCodec
  $ CJS.recordPropOptional @"build" packageBuildOptionsCodec
  $ CJS.recordPropOptional @"bundle" bundleConfigCodec
  $ CJS.recordPropOptional @"run" runConfigCodec
  $ CJS.recordPropOptional @"test" testConfigCodec
  $ CJS.recordPropOptional @"publish" publishConfigCodec
  $ CJS.record

type PublishConfig =
  { version :: Version
  , license :: License
  , location :: Maybe Location
  , include :: Maybe (Array AdHocFilePath)
  , exclude :: Maybe (Array AdHocFilePath)
  }

publishConfigCodec :: CJ.Codec PublishConfig
publishConfigCodec = CJ.named "PublishConfig" $ CJS.objectStrict
  $ CJS.recordProp @"version" Version.codec
  $ CJS.recordProp @"license" License.codec
  $ CJS.recordPropOptional @"location" publishLocationCodec
  $ CJS.recordPropOptional @"include" (CJ.array CJ.string)
  $ CJS.recordPropOptional @"exclude" (CJ.array CJ.string)
  $ CJS.record

-- This codec duplicates `Location.codec` from the Registry library, but with
-- strict parsing of fields, so that we would error out on unknown fields, thus
-- catching typos in field names. We do not want to modify the original codec in
-- the Registry library, because it's used for network communication, not for
-- reading user input, and therefore it's more important there to ignore unknown
-- fields for backwards compatibiility.
publishLocationCodec :: CJ.Codec Location
publishLocationCodec = CJ.named "Publish Location" $ Codec.codec' decode encode
  where
  decode json =
    (Location.Git <$> Codec.decode gitCodec json)
      <|> (Location.GitHub <$> Codec.decode githubCodec json)

  encode = case _ of
    Location.Git git -> CJ.encode gitCodec git
    Location.GitHub github -> CJ.encode githubCodec github

  githubCodec :: CJ.Codec Location.GitHubData
  githubCodec = Profunctor.dimap toJsonRep fromJsonRep $ CJ.named "GitHub" $ CJ.Record.objectStrict
    { githubOwner: CJ.string
    , githubRepo: CJ.string
    , subdir: CJ.Record.optional CJ.string
    }
    where
    toJsonRep { owner, repo, subdir } = { githubOwner: owner, githubRepo: repo, subdir }
    fromJsonRep { githubOwner, githubRepo, subdir } = { owner: githubOwner, repo: githubRepo, subdir }

  gitCodec :: CJ.Codec Location.GitData
  gitCodec = Profunctor.dimap toJsonRep fromJsonRep $ CJ.named "Git" $ CJ.Record.objectStrict
    { gitUrl: Reg.Internal.Codec.parsedString Reg.Internal.Parsing.gitUrl
    , subdir: CJ.Record.optional CJ.string
    }
    where
    -- The JSON representation of the GitHub type uses 'gitUrl', but in PureScript
    -- we use 'url' for convenience.
    toJsonRep { url, subdir } = { gitUrl: url, subdir }
    fromJsonRep { gitUrl, subdir } = { url: gitUrl, subdir }

type RunConfig =
  { main :: Maybe String
  , execArgs :: Maybe (Array String)
  }

runConfigCodec :: CJ.Codec RunConfig
runConfigCodec = CJ.named "RunConfig" $ CJS.objectStrict
  $ CJS.recordPropOptional @"main" CJ.string
  $ CJS.recordPropOptional @"execArgs" (CJ.array CJ.string)
  $ CJS.record

type TestConfig =
  { main :: String
  , execArgs :: Maybe (Array String)
  , dependencies :: Dependencies
  , censorTestWarnings :: Maybe CensorBuildWarnings
  , strict :: Maybe Boolean
  , pedanticPackages :: Maybe Boolean
  }

testConfigCodec :: CJ.Codec TestConfig
testConfigCodec = CJ.named "TestConfig" $ CJS.objectStrict
  $ CJS.recordProp @"main" CJ.string
  $ CJS.recordPropOptional @"execArgs" (CJ.array CJ.string)
  $ CJS.recordPropOptional @"censorTestWarnings" censorBuildWarningsCodec
  $ CJS.recordPropOptional @"strict" CJ.boolean
  $ CJS.recordPropOptional @"pedanticPackages" CJ.boolean
  $ CJS.recordProp @"dependencies" dependenciesCodec
  $ CJS.record

type BackendConfig =
  { cmd :: String
  , args :: Maybe (Array String)
  }

backendConfigCodec :: CJ.Codec BackendConfig
backendConfigCodec = CJ.named "BackendConfig" $ CJS.objectStrict
  $ CJS.recordProp @"cmd" CJ.string
  $ CJS.recordPropOptional @"args" (CJ.array CJ.string)
  $ CJS.record

type PackageBuildOptionsInput =
  { censorProjectWarnings :: Maybe CensorBuildWarnings
  , strict :: Maybe Boolean
  , pedanticPackages :: Maybe Boolean
  }

packageBuildOptionsCodec :: CJ.Codec PackageBuildOptionsInput
packageBuildOptionsCodec = CJ.named "PackageBuildOptionsInput" $ CJS.objectStrict
  $ CJS.recordPropOptional @"censorProjectWarnings" censorBuildWarningsCodec
  $ CJS.recordPropOptional @"strict" CJ.boolean
  $ CJS.recordPropOptional @"pedanticPackages" CJ.boolean
  $ CJS.record

type BundleConfig =
  { minify :: Maybe Boolean
  , module :: Maybe String
  , outfile :: Maybe AdHocFilePath
  , platform :: Maybe BundlePlatform
  , type :: Maybe BundleType
  , extraArgs :: Maybe (Array String)
  }

bundleConfigCodec :: CJ.Codec BundleConfig
bundleConfigCodec = CJ.named "BundleConfig" $ CJS.objectStrict
  $ CJS.recordPropOptional @"minify" CJ.boolean
  $ CJS.recordPropOptional @"module" CJ.string
  $ CJS.recordPropOptional @"outfile" CJ.string
  $ CJS.recordPropOptional @"platform" bundlePlatformCodec
  $ CJS.recordPropOptional @"type" bundleTypeCodec
  $ CJS.recordPropOptional @"extraArgs" (CJ.array CJ.string)
  $ CJS.record

data BundlePlatform = BundleNode | BundleBrowser

derive instance Eq BundlePlatform

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

derive instance Eq BundleType

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
  packageSingletonCodec = Reg.Internal.Codec.packageMap spagoRangeCodec

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
workspaceConfigCodec = CJ.named "WorkspaceConfig" $ CJS.objectStrict
  $ CJS.recordPropOptional @"packageSet" setAddressCodec
  $ CJS.recordPropOptional @"backend" backendConfigCodec
  $ CJS.recordPropOptional @"buildOpts" buildOptionsCodec
  $ CJS.recordPropOptional @"extraPackages" (Reg.Internal.Codec.packageMap extraPackageCodec)
  $ CJS.record

type WorkspaceBuildOptionsInput =
  { output :: Maybe AdHocFilePath
  , censorLibraryWarnings :: Maybe CensorBuildWarnings
  , statVerbosity :: Maybe StatVerbosity
  }

buildOptionsCodec :: CJ.Codec WorkspaceBuildOptionsInput
buildOptionsCodec = CJ.named "WorkspaceBuildOptionsInput" $ CJS.objectStrict
  $ CJS.recordPropOptional @"output" CJ.string
  $ CJS.recordPropOptional @"censorLibraryWarnings" censorBuildWarningsCodec
  $ CJS.recordPropOptional @"statVerbosity" statVerbosityCodec
  $ CJS.record

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

  byMessagePrefixCodec = CJ.named "ByMessagePrefix" $ CJ.Record.objectStrict { byPrefix: CJ.string }

data StatVerbosity
  = NoStats
  | CompactStats
  | VerboseStats

derive instance Eq StatVerbosity

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
  | SetFromPath { path :: AdHocFilePath }

derive instance Eq SetAddress

setAddressCodec :: CJ.Codec SetAddress
setAddressCodec = Codec.codec' decode encode
  where
  setFromRegistryCodec = CJ.named "SetFromRegistry" $ CJ.Record.objectStrict { registry: Version.codec }
  setFromUrlCodec = CJ.named "SetFromUrl" $ CJ.Record.objectStrict { url: CJ.string, hash: CJ.Record.optional Sha256.codec }
  setFromPathCodec = CJ.named "SetFromPath" $ CJ.Record.objectStrict { path: CJ.string }

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

type LocalPackage = { path :: AdHocFilePath }

localPackageCodec :: CJ.Codec LocalPackage
localPackageCodec = CJ.named "LocalPackage" $ CJ.Record.objectStrict { path: CJ.string }

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
  , subdir :: Maybe AdHocFilePath
  , dependencies :: Maybe Dependencies
  }

gitPackageCodec :: CJ.Codec GitPackage
gitPackageCodec = CJ.named "GitPackage" $ CJS.objectStrict
  $ CJS.recordProp @"git" CJ.string
  $ CJS.recordProp @"ref" CJ.string
  $ CJS.recordPropOptional @"subdir" CJ.string
  $ CJS.recordPropOptional @"dependencies" dependenciesCodec
  $ CJS.record

-- | The format of a legacy packages.json package set entry for an individual
-- | package.
type LegacyPackageSetEntry =
  { dependencies :: Array PackageName
  , repo :: String
  , version :: String
  }

legacyPackageSetEntryCodec :: CJ.Codec LegacyPackageSetEntry
legacyPackageSetEntryCodec = CJ.named "LegacyPackageSetEntry" $ CJS.objectStrict
  $ CJS.recordProp @"repo" CJ.string
  $ CJS.recordProp @"version" CJ.string
  $ CJS.recordProp @"dependencies" (CJ.array PackageName.codec)
  $ CJS.record
