module Src.PACKAGE.A where

import Prelude
import Src.PACKAGE.SHARED as PACKAGE.SHARED

libraryUsage :: String
libraryUsage = packageNameValue <> PACKAGE.SHARED.packageNameValue

packageNameValue :: String
packageNameValue = "package name " <> "package-a"