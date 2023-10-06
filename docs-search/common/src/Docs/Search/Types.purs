module Docs.Search.Types
  ( module ReExport
  , packageNameCodec
  , moduleNameCodec
  , Identifier(..)
  , PackageInfo(..)
  , packageInfoCodec
  , PackageScore(..)
  , packageScoreCodec
  , GlobalIdentifier(..)
  , PartId(..)
  , URL(..)
  , FilePath(..)
  ) where

import Web.Bower.PackageMeta (PackageName(..)) as ReExport
import Language.PureScript.Names (ModuleName(..)) as ReExport

import Docs.Search.JsonCodec (inject)

import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Sum as CAS
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..))
import Data.Profunctor (wrapIso, dimap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Language.PureScript.Names (ModuleName(..))
import Web.Bower.PackageMeta (PackageName(..))

newtype Identifier = Identifier String

derive instance newtypeIdentifier :: Newtype Identifier _
derive instance genericIdentifier :: Generic Identifier _
derive newtype instance eqIdentifier :: Eq Identifier
derive newtype instance ordIdentifier :: Ord Identifier
derive newtype instance showIdentifier :: Show Identifier

moduleNameCodec :: JsonCodec ModuleName
moduleNameCodec = wrapIso ModuleName CA.string

data PackageInfo = LocalPackage | Builtin | Package PackageName | UnknownPackage

derive instance eqPackageInfo :: Eq PackageInfo
derive instance ordPackageInfo :: Ord PackageInfo
derive instance genericPackageInfo :: Generic PackageInfo _
instance showPackageInfo :: Show PackageInfo where
  show = genericShow

packageNameCodec :: JsonCodec PackageName
packageNameCodec = wrapIso PackageName CA.string

packageInfoCodec :: JsonCodec PackageInfo
packageInfoCodec =
  dimap toVariant fromVariant $ CAV.variantMatch
    { local: Left unit
    , builtin: Left unit
    , unknown: Left unit
    , package: Right packageNameCodec
    }
  where
  toVariant = case _ of
    LocalPackage -> inject @"local" unit
    Builtin -> inject @"builtin" unit
    Package name -> inject @"package" name
    UnknownPackage -> inject @"unknown" unit

  fromVariant = Variant.match
    { local: \_ -> LocalPackage
    , builtin: \_ -> Builtin
    , unknown: \_ -> UnknownPackage
    , package: \name -> Package name
    }

newtype PackageScore = PackageScore Int

derive instance newtypePackageScore :: Newtype PackageScore _
derive instance genericPackageScore :: Generic PackageScore _
derive newtype instance eqPackageScore :: Eq PackageScore
derive newtype instance ordPackageScore :: Ord PackageScore
derive newtype instance semiringPackageScore :: Semiring PackageScore
derive newtype instance ringPackageScore :: Ring PackageScore
derive newtype instance showPackageScore :: Show PackageScore

packageScoreCodec :: JsonCodec PackageScore
packageScoreCodec = wrapIso PackageScore CA.int

newtype URL = URL String

derive instance newtypeURL :: Newtype URL _
derive newtype instance showURL :: Show URL

newtype FilePath = FilePath String

derive instance newtypeFilePath :: Newtype FilePath _
derive newtype instance showFilePath :: Show FilePath

newtype GlobalIdentifier = GlobalIdentifier String

derive instance newtypeGlobalIdentifier :: Newtype GlobalIdentifier _
derive newtype instance showGlobalIdentifier :: Show GlobalIdentifier

newtype PartId = PartId Int

derive instance newtypePartId :: Newtype PartId _
derive newtype instance eqPartId :: Eq PartId
derive newtype instance ordPartId :: Ord PartId
derive newtype instance showPartId :: Show PartId
