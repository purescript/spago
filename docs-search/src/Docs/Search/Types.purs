module Docs.Search.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)


newtype Identifier = Identifier String

derive instance newtypeIdentifier :: Newtype Identifier _
derive instance genericIdentifier :: Generic Identifier _
derive newtype instance eqIdentifier :: Eq Identifier
derive newtype instance ordIdentifier :: Ord Identifier
derive newtype instance showIdentifier :: Show Identifier
derive newtype instance decodeJsonIdentifier :: DecodeJson Identifier
derive newtype instance encodeJsonIdentifier :: EncodeJson Identifier


newtype ModuleName = ModuleName String

derive instance newtypeModuleName :: Newtype ModuleName _
derive instance genericModuleName :: Generic ModuleName _
derive newtype instance eqModuleName :: Eq ModuleName
derive newtype instance ordModuleName :: Ord ModuleName
derive newtype instance decodeJsonModuleName :: DecodeJson ModuleName
derive newtype instance encodeJsonModuleName :: EncodeJson ModuleName

instance Show ModuleName where
  show = genericShow


-- | Non-normalized package name, e.g. `purescript-prelude` or just `prelude`.
newtype RawPackageName = RawPackageName String

derive instance newtypeRawPackageName :: Newtype RawPackageName _


-- | Normalized package name without "purescript-" prefix.
newtype PackageName = PackageName String

derive instance newtypePackageName :: Newtype PackageName _
derive newtype instance eqPackageName :: Eq PackageName
derive newtype instance ordPackageName :: Ord PackageName
derive newtype instance showPackageName :: Show PackageName
derive newtype instance decodeJsonPackageName :: DecodeJson PackageName
derive newtype instance encodeJsonPackageName :: EncodeJson PackageName
derive instance genericPackageName :: Generic PackageName _


data PackageInfo = LocalPackage | Builtin | Package PackageName | UnknownPackage

derive instance eqPackageInfo :: Eq PackageInfo
derive instance ordPackageInfo :: Ord PackageInfo
derive instance genericPackageInfo :: Generic PackageInfo _
instance decodeJsonPackageInfo :: DecodeJson PackageInfo where
  decodeJson = genericDecodeJson
instance encodeJsonPackageInfo :: EncodeJson PackageInfo where
  encodeJson = genericEncodeJson

instance showPackageInfo :: Show PackageInfo where
  show = genericShow


newtype PackageScore = PackageScore Int

derive instance newtypePackageScore :: Newtype PackageScore _
derive instance genericPackageScore :: Generic PackageScore _
derive newtype instance eqPackageScore :: Eq PackageScore
derive newtype instance ordPackageScore :: Ord PackageScore
derive newtype instance semiringPackageScore :: Semiring PackageScore
derive newtype instance ringPackageScore :: Ring PackageScore
derive newtype instance showPackageScore :: Show PackageScore
derive newtype instance decodeJsonPackageScore :: DecodeJson PackageScore
derive newtype instance encodeJsonPackageScore :: EncodeJson PackageScore


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
