module Src.PACKAGE.B where

import Prelude

libraryUsage :: String
libraryUsage = packageNameValue <> "no deps"

packageNameValue :: String
packageNameValue = "package name " <> "package-b"