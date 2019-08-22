module Spago.Git
  ( requireCleanWorkingTree
  , hasCleanWorkingTree
  , getAllTags
  , getCurrentVersion
  , commitAndTag
  , pushTag
  , isIgnored
  , parseVersion
  , unparseVersion
  ) where

import           Spago.Prelude

import qualified Data.Text     as Text
import           Data.Versions (SemVer (..))
import qualified Data.Versions as Version
import qualified Safe.Foldable as Safe



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

  tagTexts <- getAllTags
  let tags = catMaybes $ hush . parseVersion <$> tagTexts

  case Safe.maximumMay tags of
    Nothing -> do
      echo $ "No git version tags found, so assuming current version is " <> unparseVersion mempty
      pure mempty
    Just maxVersion -> do
      echo $ "Found current version from git tag: " <> unparseVersion maxVersion
      pure maxVersion


requireCleanWorkingTree :: Spago m => m ()
requireCleanWorkingTree = do
  clean <- hasCleanWorkingTree
  unless clean $ do
    die "Your git working tree is dirty. Please commit or stash your changes first."


hasCleanWorkingTree :: Spago m => m Bool
hasCleanWorkingTree = do
  (code, stdout, stderr) <- procStrictWithErr "git" ["status", "--porcelain"] empty

  when (code /= ExitSuccess) $ do
    echoDebug $ "git status stderr: " <> stderr
    die "Unable to check git status. Perhaps git is not installed or this is not a git repository?"

  pure $ stdout == ""


getAllTags :: Spago m => m [Text]
getAllTags = do
  fmap Text.lines $ strict $ inproc "git" ["tag", "--list"] empty


commitAndTag :: Spago m => Text -> Text -> m ()
commitAndTag tag message = do
  procs "git" ["commit", "--quiet", "--allow-empty", "--message=" <> message] empty
  procs "git" ["tag", "--annotate", "--message=" <> message, tag] empty


pushTag :: Spago m => Text -> m ()
pushTag tag = do
  procs "git" ["push", "origin", "HEAD", "refs/tags/" <> tag] empty


isIgnored :: Spago m => Text -> m Bool
isIgnored path = do
  (== ExitSuccess) <$> proc "git" ["check-ignore", "--quiet", path] empty
