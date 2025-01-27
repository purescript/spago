module Spago.Command.Resolution where

import Prelude

import Data.Map (Map, lookup, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Version (Version)
import Registry.Version as Version
import Spago.Config (Package(..))

type VersionResolution =
  { name :: PackageName
  , requested :: Range
  , resolved :: Version
  }

resolutions :: Map PackageName Package -> Map PackageName Range -> Array VersionResolution
resolutions registry dependencies = do
  Tuple name requested <- toUnfoldable dependencies
  case (lookup name registry) of
    Just (RegistryVersion resolved) -> [ { name, requested, resolved } ]
    _ -> []

available :: VersionResolution -> Boolean
available { requested, resolved } = Range.includes requested resolved

unavailable :: VersionResolution -> Boolean
unavailable = not <<< available

list :: PackageName -> String -> String
list k v = "  - " <> PackageName.print k <> ": " <> v

missingWarning :: Array VersionResolution -> String
missingWarning missing = joinWith "\n" $ join
  [ [ "The following packages versions do not exist in your package set:" ]
  , (\{ name, requested } -> list name $ Range.print requested) <$> missing
  , [ "", "Proceeding with the latest available versions instead:" ]
  , (\{ name, resolved } -> list name $ Version.print resolved) <$> missing
  ]
