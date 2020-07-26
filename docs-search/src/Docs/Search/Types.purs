module Docs.Search.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)


newtype ModuleName = ModuleName String

derive instance newtypeModuleName :: Newtype ModuleName _
derive instance genericModuleName :: Generic ModuleName _
derive newtype instance eqModuleName :: Eq ModuleName
derive newtype instance ordModuleName :: Ord ModuleName
derive newtype instance decodeJsonModuleName :: DecodeJson ModuleName
derive newtype instance encodeJsonModuleName :: EncodeJson ModuleName


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


data PackageInfo = Package PackageName | Builtin | LocalPackage | UnknownPackage

derive instance eqPackageInfo :: Eq PackageInfo
derive instance ordPackageInfo :: Ord PackageInfo
derive instance genericPackageInfo :: Generic PackageInfo _
instance showPackageInfo :: Show PackageInfo where
  show = genericShow

instance decodeJsonPackageInfo :: DecodeJson PackageInfo where
  decodeJson = genericDecodeJson
instance encodeJsonPackageInfo :: EncodeJson PackageInfo where
  encodeJson = genericEncodeJson

packageInfoToString :: PackageInfo -> String
packageInfoToString (Package (PackageName p)) = p
packageInfoToString Builtin = "<builtin>"
packageInfoToString LocalPackage = "<local package>"
packageInfoToString UnknownPackage = "<unknown package>"
