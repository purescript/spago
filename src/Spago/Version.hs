{-# LaNgUaGe OverloadedLists #-}
module Spago.Version
  ( VersionBump(..)
  , DryRun(..)
  , bumpVersion
  , getNextVersion
  , parseVersion
  , parseVersionBump
  , unparseVersion
  ) where

import           Spago.Prelude

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text            as Text
import           Data.Versions        (SemVer (..))
import qualified Data.Versions        as Version
import qualified Safe.Foldable        as Safe

import qualified Spago.Bower          as Bower
import           Spago.DryRun         (DryAction (..), DryRun (..), runDryActions)
import qualified Spago.Git            as Git


data VersionBump
  = Major
  | Minor
  | Patch
  | Exact SemVer
  deriving (Eq, Show)


parseVersionBump :: Text -> Maybe VersionBump
parseVersionBump = \case
  "major" -> Just Major
  "minor" -> Just Minor
  "patch" -> Just Patch
  v | Right v' <- parseVersion v -> Just $ Exact v'
  _ -> Nothing


-- | Parses a version, ignoring an optional leading 'v', or returns an error message.
parseVersion :: Text -> Either Version.ParsingError SemVer
parseVersion =
  Version.semver . Text.dropWhile (== 'v')


-- | Turns a version into text, with a leading 'v'.
unparseVersion :: SemVer -> Text
unparseVersion version =
  "v" <> Version.prettySemVer version


-- | Get the highest git version tag, die if this is not a git repo with no uncommitted changes.
getCurrentVersion :: HasLogFunc env => RIO env SemVer
getCurrentVersion = do

  tagTexts <- Git.getAllTags
  let tags = catMaybes $ hush . parseVersion <$> tagTexts

  case Safe.maximumMay tags of
    Nothing -> do
      logInfo $ display $ "No git version tags found, so assuming current version is " <> unparseVersion mempty
      pure mempty
    Just maxVersion -> do
      logInfo $ display $ "Found current version from git tag: " <> unparseVersion maxVersion
      pure maxVersion


getNextVersion :: VersionBump -> SemVer -> Either Text SemVer
getNextVersion spec currentV@SemVer{..} =
  case spec of
    Major -> Right $ SemVer (_svMajor + 1) 0 0 [] []
    Minor -> Right $ SemVer _svMajor (_svMinor + 1) 0 [] []
    Patch -> Right $ SemVer _svMajor _svMinor (_svPatch + 1) [] []
    Exact newV
      | currentV < newV -> Right newV
      | otherwise -> do
        let new = unparseVersion newV
            current = unparseVersion currentV
        Left $ "The new version (" <> new <> ") must be higher than the current version (" <> current <> ")"


-- | Make a tag for the new version.
tagNewVersion :: HasLogFunc env => SemVer -> SemVer -> RIO env ()
tagNewVersion oldVersion newVersion = do

  let oldVersionTag = unparseVersion oldVersion
      newVersionTag = unparseVersion newVersion

  Git.commitAndTag newVersionTag $ oldVersionTag <> " → " <> newVersionTag
  logInfo $ display $ "Git tag created for new version: " <> newVersionTag


-- | Bump and tag a new version in preparation for release.
bumpVersion
  :: (HasLogFunc env, HasConfigPath env, HasJobs env)
  => DryRun -> VersionBump 
  -> RIO env ()
bumpVersion dryRun spec = do
  newBowerConfig <- Bower.generateBowerJson

  Git.requireCleanWorkingTree

  oldVersion <- getCurrentVersion
  newVersion <- either (\err -> die [ display err ]) pure $ getNextVersion spec oldVersion

  let writeBowerAction = DryAction
        "write the new config to the `bower.json` file and try to install its dependencies" $ do
        logInfo $ "Writing the new Bower config to " <> surroundQuote Bower.path
        liftIO $ ByteString.writeFile Bower.path newBowerConfig
        Bower.runBowerInstall
        clean <- Git.hasCleanWorkingTree
        unless clean $ do
          die [ "A new " <> Bower.path <> " has been generated. Please commit this and run `bump-version` again." ]

  let tagAction = DryAction
        ("create (locally) the new git tag " <> surroundQuote (unparseVersion newVersion))
        (tagNewVersion oldVersion newVersion)

  runDryActions dryRun
    [ writeBowerAction
    , tagAction 
    ]
