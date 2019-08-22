{-# LANGUAGE OverloadedLists #-}
module Spago.Version
  ( VersionBump(..)
  , DryRun(..)
  , bumpVersion
  ) where

import           Spago.Prelude

import qualified Data.ByteString.Lazy as ByteString
import           Data.Versions        (SemVer (..))

import qualified Spago.Bower          as Bower
import qualified Spago.Config         as Config
import           Spago.DryRun         (DryAction (..), DryRun (..), runDryActions)
import qualified Spago.Git            as Git


data VersionBump
  = Major
  | Minor
  | Patch
  | Exact SemVer


-- | Get the next version to use, or die if this would result in the version number going down/not changing.
getNextVersion :: Spago m => VersionBump -> SemVer -> m SemVer
getNextVersion spec version@SemVer{..} =
  case spec of
    Major -> pure $ SemVer (_svMajor + 1) 0 0 [] []
    Minor -> pure $ SemVer _svMajor (_svMinor + 1) 0 [] []
    Patch -> pure $ SemVer _svMajor _svMinor (_svPatch + 1) [] []
    Exact v
      | v > version -> pure v
      | otherwise -> do
        let new = Git.unparseVersion v
            old = Git.unparseVersion version
        die $ "The new version (" <> new <> ") must be higher than the current version (" <> old <> ")"


-- | Make a tag for the new version.
tagNewVersion :: Spago m => SemVer -> SemVer -> m ()
tagNewVersion oldVersion newVersion = do

  let oldVersionTag = Git.unparseVersion oldVersion
      newVersionTag = Git.unparseVersion newVersion

  Git.commitAndTag newVersionTag $ oldVersionTag <> " → " <> newVersionTag
  echo $ "Git tag created for new version: " <> newVersionTag


-- | Bump and tag a new version in preparation for release.
bumpVersion :: Spago m => DryRun -> VersionBump -> m ()
bumpVersion dryRun spec = do
  config <- Config.ensureConfig
  newBowerConfig <- Bower.generateBowerJson config

  Git.requireCleanWorkingTree

  oldVersion <- Git.getCurrentVersion
  newVersion <- getNextVersion spec oldVersion

  let writeBowerAction = DryAction
        ("write the new config to the `bower.json` file and try to install its dependencies") $ do
        echo $ "Writing the new Bower config to " <> surroundQuote Bower.path
        liftIO $ ByteString.writeFile Bower.path newBowerConfig
        Bower.runBowerInstall
        clean <- Git.hasCleanWorkingTree
        when (not clean) $ do
          die $ "A new " <> Bower.path <> " has been generated. Please commit this and run `bump-version` again."

  let tagAction = DryAction
        ("create (locally) the new git tag " <> surroundQuote (Git.unparseVersion newVersion))
        (tagNewVersion oldVersion newVersion)

  runDryActions dryRun
    [ writeBowerAction
    , tagAction
    ]
    (echo "Done 🎉")
