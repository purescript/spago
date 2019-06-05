module Spago.Messages where

import Spago.Prelude

import qualified Data.Text as Text


cannotFindConfig :: Text
cannotFindConfig = makeMessage
  [ "There's no " <> surroundQuote "spago.dhall" <> " in your current location."
  , ""
  , "If you already have a spago project you might be in the wrong subdirectory,"
  , "otherwise you might want to run `spago init` to initialize a new project."
  ]

cannotFindPackages :: Text
cannotFindPackages = makeMessage
  [ "There's no " <> surroundQuote "packages.dhall" <> " in your current location."
  , ""
  , "If you already have a spago project you might be in the wrong subdirectory,"
  , "otherwise you might want to run `spago init` to initialize a new package set file."
  ]

cannotFindPackagesButItsFine :: Text
cannotFindPackagesButItsFine = makeMessage
  [ "WARNING: did not find a " <> surroundQuote "packages.dhall" <> " in your current location, skipping compiler version check"
  ]

foundExistingProject :: Text -> Text
foundExistingProject pathText = makeMessage
  [ "Found a " <> surroundQuote pathText <> " file, skipping copy. Run `spago init --force` if you wish to overwrite it."
  ]

foundExistingDirectory :: Text -> Text
foundExistingDirectory dir
  = "Found existing directory " <> surroundQuote dir <> ", skipping copy of sample sources"

foundExistingFile :: Text -> Text
foundExistingFile file
  = "Found existing file " <> surroundQuote file <> ", not overwriting it"

foundLocalPackage :: Text -> Text -> Text
foundLocalPackage package path
  = "Skipping package " <> surroundQuote package <> ", using local path: " <> surroundQuote path

failedToInstallDep :: Text -> Text -> Text
failedToInstallDep dep stderr = makeMessage
  [ ""
  , "Failed to install dependency " <> dep
  , "Git output:"
  , ""
  , stderr
  , ""
  , "Aborting installation.."
  ]

failedToReadFile :: Text -> Text
failedToReadFile file = makeMessage
  [ "Failed to read " <> surroundQuote file ]

failedToReadPscFile :: Show a => a -> Text
failedToReadPscFile err = makeMessage
  [ "WARNING: found a \"psc-package.json\" file, but was not"
  , "able to read it, skipping the automatic import.."
  , "This is the error we got:"
  , ""
  , tshow err
  , ""
  ]

failedToParseFile :: Show a => Text -> a -> Text
failedToParseFile file err = makeMessage
  [ "Error while trying to parse " <> surroundQuote file
  , "Details:"
  , ""
  , tshow err
  ]

failedToParseCommandOutput :: Text -> Text -> Text
failedToParseCommandOutput command output = makeMessage
  [ "Failed to parse '" <> command <> "' output: "
  , surroundQuote output
  ]

failedToReachGitHub :: Show a => a -> Text
failedToReachGitHub err = makeMessage
  [ "Could not reach GitHub. Error:"
  , ""
  , tshow err
  ]

failedToAddDeps :: [Text] -> Text
failedToAddDeps pkgs = makeMessage $
  [ "Some of the dependencies you tried to add were not found in the package-set."
  , "Not adding any new dependencies to your new spago config."
  , "We didn't find:"
  ]
  <> map ((<>) "- ") pkgs
  <> [""]

upgradingPackageSet :: Text -> Text
upgradingPackageSet newTag = makeMessage
  [ "Package-set upgraded to latest tag " <> surroundQuote newTag
  , "Fetching the new one and generating hashes.. (this might take some time)"
  ]

freezePackageSet :: Text
freezePackageSet = makeMessage
  [ "Generating new hashes for the package set file so it will be cached.. (this might take some time)"
  ]

packageSetVersionWarning :: Text
packageSetVersionWarning = makeMessage
 [ "WARNING: the package-set version you're on doesn't check if the version of the"
 , "PureScript compiler installed on your system is compatible."
 , "If your build fails you might want to upgrade your set by running this command:"
 , "`spago package-set-upgrade`"
 , ""
 ]

pursVersionMismatch :: Text -> Text -> Text
pursVersionMismatch currentVersion minVersion = makeMessage
  [ "Oh noes! It looks like the PureScript version installed on your system is not compatible with the package-set you're using."
  , ""
  , "installed version:   " <> currentVersion
  , "package-set version: " <> minVersion
  , ""
  , "There are a few ways to solve this:"
  , "- install a compatible `purs` version (i.e. in the same 'semver range')"
  , "- if you know what you're doing, you can override the `version` of the `metadata` package in the packages.dhall"
  , ""
  ]

verifying :: Show a => a -> Text
verifying len = "Verifying " <> tshow len <> " packages, this might take a while.."

bundleCommandRenamed :: Text
bundleCommandRenamed =
  "The `bundle` command has been replaced with `bundle-app`, so use that instead."

makeModuleCommandRenamed :: Text
makeModuleCommandRenamed =
  "The `make-module` command has been replaced with `bundle-module`, so use that instead."

surroundQuote :: Text -> Text
surroundQuote y = "\"" <> y <> "\""

makeMessage :: [Text] -> Text
makeMessage = Text.intercalate "\n"
