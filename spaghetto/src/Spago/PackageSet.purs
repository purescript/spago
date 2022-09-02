module Spago.PackageSet where

import Spago.Prelude

import Registry.PackageName (PackageName)
import Registry.Version (Version)

type PackageSet = Map PackageName Package

-- TODO: local packages too
data Package
  = Version Version
  | GitPackage GitPackage

instance Show Package where
  show = case _ of
    Version v -> show v
    GitPackage p -> show p

type GitPackage =
  { git :: String
  , ref :: String
  , dependencies :: Maybe (Array PackageName)
  }
