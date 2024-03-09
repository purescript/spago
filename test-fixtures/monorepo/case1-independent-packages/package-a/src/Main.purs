module Src.PACKAGE.A where

import Prelude

libraryUsage :: String
libraryUsage = packageNameValue <> "no deps"

packageNameValue :: String
packageNameValue = "package name " <> "package-a"