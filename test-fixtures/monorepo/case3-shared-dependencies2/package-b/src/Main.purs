module Src.PACKAGE.B where

import Prelude
import Src.PACKAGE.C as PACKAGE.C

libraryUsage :: String
libraryUsage = packageNameValue <> PACKAGE.C.packageNameValue

packageNameValue :: String
packageNameValue = "package name " <> "package-b"