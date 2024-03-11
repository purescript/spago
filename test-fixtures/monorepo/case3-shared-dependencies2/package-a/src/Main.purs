module Src.PACKAGE.A where

import Prelude
import Src.PACKAGE.C as PACKAGE.C
import Src.PACKAGE.B as PACKAGE.B

libraryUsage :: String
libraryUsage = packageNameValue <> PACKAGE.C.packageNameValue <> PACKAGE.B.packageNameValue

packageNameValue :: String
packageNameValue = "package name " <> "package-a"