module Src.PACKAGE.B where

import Prelude
import Data.Either

libraryUsage :: String
libraryUsage = packageNameValue <> EITHER.packageNameValue

packageNameValue :: String
packageNameValue = "package name " <> "package-b"