module Spago.Version
  ( VersionBump(..)
  , bumpVersion
  , parseVersion
  , unparseVersion
  ) where

import           Spago.Prelude

import           Data.SemVer   (Version)
import qualified Data.SemVer   as SemVer
import qualified Data.Text     as Text
import qualified Safe.Foldable as Safe

import qualified Spago.Bower   as Bower
import qualified Spago.Git     as Git


data VersionBump
  = Major
  | Minor
  | Patch
  | Exact Version


-- | Parses a version, ignoring an optional leading 'v', or returns an error message.
parseVersion :: Text -> Either String Version
parseVersion =
  SemVer.fromText . Text.dropWhile (== 'v')


-- | Turns a version into text, with a leading 'v'.
unparseVersion :: Version -> Text
unparseVersion version =
  "v" <> SemVer.toText version


-- | Get the highest git version tag, die if this is not a git repo with no uncommitted changes.
getCurrentVersion :: Spago m => m Version
getCurrentVersion = do
  Git.requireCleanWorkingTree

  tagTexts <- Git.getAllTags
  let tags = catMaybes $ hush . parseVersion <$> tagTexts

  case Safe.maximumMay tags of
    Nothing -> do
      echo $ "No existing version tags found so assuming current version is " <> unparseVersion SemVer.initial
      pure SemVer.initial
    Just maxVersion -> do
      echo $ "Found current version from git tag: " <> unparseVersion maxVersion
      pure maxVersion


-- | Get the next version to use, or die if this would result in the version number going down/not changing.
getNextVersion :: Spago m => VersionBump -> Version -> m Version
getNextVersion spec version =
  case spec of
    Major -> pure $ SemVer.incrementMajor version
    Minor -> pure $ SemVer.incrementMinor version
    Patch -> pure $ SemVer.incrementPatch version
    Exact v
      | v > version -> pure v
      | otherwise -> die "Oh noes! The new version must be higher than the current version." -- todo: move to messages module


-- | Make a tag for the new version.
tagNewVersion :: Spago m => Version -> Version -> m ()
tagNewVersion oldVersion newVersion = do

  let oldVersionTag = unparseVersion oldVersion
      newVersionTag = unparseVersion newVersion

  Git.commitAndTag newVersionTag $ oldVersionTag <> " → " <> newVersionTag
  echo $ "Git tag created for new version: " <> SemVer.toText newVersion


-- | Bump and tag a new version in preparation for release.
bumpVersion :: Spago m => VersionBump -> m ()
bumpVersion spec = do
  oldVersion <- getCurrentVersion
  newVersion <- getNextVersion spec oldVersion
  Bower.writeBowerJson
  Git.addAllChanges
  tagNewVersion oldVersion newVersion
