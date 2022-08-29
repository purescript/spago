module Spago.Config where

import Spago.Prelude

type Config =
  { name :: String
  , dependencies :: Map String String
  , package_db :: PackagesDb
  }

type PackagesDb =
  { set :: String
  , extra_packages :: Map String ExtraPackage
  }

-- TODO: local packages too
type ExtraPackage =
  { git :: String
  , ref :: String
  }

-- TODO alternateBackend
-- TODO publish config
