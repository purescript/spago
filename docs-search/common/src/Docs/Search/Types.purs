module Docs.Search.Types
  ( module ReExport
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

import Prelude

import Data.Codec.JSON.Common as CJ
import Data.Codec.JSON.Variant as CJ.Variant
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso, dimap)
import Data.Variant as Variant
import Docs.Search.JsonCodec (inject)
import Language.PureScript.Names (ModuleName(..))
import Language.PureScript.Names (ModuleName(..)) as ReExport
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName

newtype Identifier = Identifier String

derive instance newtypeIdentifier :: Newtype Identifier _
derive instance genericIdentifier :: Generic Identifier _
derive newtype instance eqIdentifier :: Eq Identifier
derive newtype instance ordIdentifier :: Ord Identifier
derive newtype instance showIdentifier :: Show Identifier

moduleNameCodec :: CJ.Codec ModuleName
moduleNameCodec = wrapIso ModuleName CJ.string

data PackageInfo
  = LocalPackage PackageName
  | Package PackageName
  | Builtin
  | UnknownPackage

derive instance eqPackageInfo :: Eq PackageInfo
derive instance ordPackageInfo :: Ord PackageInfo
derive instance genericPackageInfo :: Generic PackageInfo _

instance Show PackageInfo where
  show = case _ of
    LocalPackage name -> "LocalPackage " <> PackageName.print name
    Package name -> "Package " <> PackageName.print name
    Builtin -> "Builtin"
    UnknownPackage -> "UnknownPackage"

packageInfoCodec :: CJ.Codec PackageInfo
packageInfoCodec =
  dimap toVariant fromVariant $ CJ.Variant.variantMatch
    { local: Right PackageName.codec
    , builtin: Left unit
    , unknown: Left unit
    , package: Right PackageName.codec
    }
  where
  toVariant = case _ of
    LocalPackage name -> inject @"local" name
    Builtin -> inject @"builtin" unit
    Package name -> inject @"package" name
    UnknownPackage -> inject @"unknown" unit

  fromVariant = Variant.match
    { local: \name -> LocalPackage name
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

packageScoreCodec :: CJ.Codec PackageScore
packageScoreCodec = wrapIso PackageScore CJ.int

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
