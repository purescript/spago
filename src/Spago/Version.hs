module Spago.Version
  ( VersionBump(..)
  , DryRun(..)
  , bumpVersion
  , parseVersion
  , unparseVersion
  ) where

import           Spago.Prelude

import           Data.Versions    (SemVer (..))
import qualified Data.Versions as Version
import qualified Data.Text     as Text
import qualified Safe.Foldable as Safe

import qualified Spago.Bower   as Bower
import           Spago.DryRun     (DryRun(..), DryAction(..), runDryActions)
import qualified Spago.Git     as Git
import Spago.Messages (surroundQuote)


data VersionBump
  = Major
  | Minor
  | Patch
  | Exact SemVer


-- | Parses a version, ignoring an optional leading 'v', or returns an error message.
parseVersion :: Text -> Either Version.ParsingError SemVer
parseVersion =
  Version.semver . Text.dropWhile (== 'v')


-- | Turns a version into text, with a leading 'v'.
unparseVersion :: SemVer -> Text
unparseVersion version =
  "v" <> Version.prettySemVer version


-- | Get the highest git version tag, die if this is not a git repo with no uncommitted changes.
getCurrentVersion :: Spago m => m SemVer
getCurrentVersion = do

  tagTexts <- Git.getAllTags
  let tags = catMaybes $ hush . parseVersion <$> tagTexts

  case Safe.maximumMay tags of
    Nothing -> do
      echo $ "No git version tags found, so assuming current version is " <> unparseVersion mempty
      pure mempty
    Just maxVersion -> do
      echo $ "Found current version from git tag: " <> unparseVersion maxVersion
      pure maxVersion


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
        let new = unparseVersion v
            old = unparseVersion version
        die $ "The new version (" <> new <> ") must be higher than the current version (" <> old <> ")"


-- | Make a tag for the new version.
tagNewVersion :: Spago m => SemVer -> SemVer -> m ()
tagNewVersion oldVersion newVersion = do

  let oldVersionTag = unparseVersion oldVersion
      newVersionTag = unparseVersion newVersion

  Git.commitAndTag newVersionTag $ oldVersionTag <> " → " <> newVersionTag
  echo $ "Git tag created for new version: " <> newVersionTag


-- | Bump and tag a new version in preparation for release.
bumpVersion :: Spago m => DryRun -> VersionBump -> m ()
bumpVersion dryRun spec = do
  Git.requireCleanWorkingTree

  oldVersion <- getCurrentVersion
  newVersion <- getNextVersion spec oldVersion

  let writeBowerAction = DryAction
        ("generate `bower.json` from your config and try to install its dependencies") $ do
        Bower.writeBowerJson
        Bower.runBowerInstall
        clean <- Git.hasCleanWorkingTree
        when (not clean) $ do
          die $ "A new " <> Bower.bowerPath <> " has been generated. Please commit this and run `bump-version` again."

  let tagAction = DryAction
        ("create (locally) the new git tag " <> surroundQuote (unparseVersion newVersion))
        (tagNewVersion oldVersion newVersion)

  runDryActions dryRun
    $ writeBowerAction
    :| [ tagAction ]
