module Src.PACKAGE.C where

import Prelude

libraryUsage :: String
libraryUsage = packageNameValue <> "no deps"

packageNameValue :: String
packageNameValue = "package name " <> "package-c"