module Spago.Version
  ( VersionBump(..)
  , VersionTriple
  , bumpVersion
  , parseVersion
  , unparseVersion
  ) where

import           Spago.Prelude

import qualified Data.Text                  as Text
import           Data.Void                  (Void)
import qualified Safe.Foldable              as Safe
import           Text.Megaparsec            (ParseErrorBundle, Parsec)
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Spago.Bower                as Bower
import qualified Spago.Git                  as Git


type VersionTriple = (Integer, Integer, Integer)

data VersionBump
  = Major
  | Minor
  | Patch
  | Exact VersionTriple


versionParser :: Parsec Void Text VersionTriple
versionParser = do
  MP.optional $ MP.single 'v'
  major <- L.decimal
  MP.single '.'
  minor <- L.decimal
  MP.single '.'
  patch <- L.decimal
  MP.eof
  pure (major, minor, patch)


parseVersion :: Text -> Either (ParseErrorBundle Text Void) VersionTriple
parseVersion =
  MP.parse versionParser ""


unparseVersion :: VersionTriple -> Text
unparseVersion (major, minor, patch) =
  Text.pack $ "v" <> show major <> "." <> show minor <> "." <> show patch


-- | Get the highest git version tag, die if this is not a git repo with no uncommitted changes.
getCurrentVersion :: Spago m => m VersionTriple
getCurrentVersion = do
  Git.requireCleanWorkingTree

  tagTexts <- Git.getAllTags
  let tags = catMaybes $ hush . parseVersion <$> tagTexts

  case Safe.maximumMay tags of
    Nothing -> do
      echo "No existing version tags found so assuming current version is v0.0.0"
      pure (0, 0, 0)
    Just maxTag -> do
      echo $ "Found current version from git tag: " <> unparseVersion maxTag
      pure maxTag


-- | Get the next version to use, or die if this would result in the version number going down/not changing.
getNextVersion :: Spago m => VersionBump -> VersionTriple -> m VersionTriple
getNextVersion spec v@(major, minor, patch) =
  case spec of
    Major -> pure (major + 1, 0        , 0        )
    Minor -> pure (major    , minor + 1, 0        )
    Patch -> pure (major    , minor    , patch + 1)
    Exact v'
      | v' > v    -> pure v'
      | otherwise -> die "Oh noes! The new version must be higher than the current version." -- todo: move to messages module


-- | Make a tag for the new version
tagNewVersion :: Spago m => VersionTriple -> VersionTriple -> m ()
tagNewVersion oldVersion newVersion = do

  let oldVersionStr = unparseVersion oldVersion
      newVersionStr = unparseVersion newVersion

  Git.commitAndTag newVersionStr $ oldVersionStr <> " → " <> newVersionStr
  echo $ "Git tag created for new version: " <> newVersionStr


-- | Bump and tag a new version in preparation for release.
bumpVersion :: Spago m => VersionBump -> m ()
bumpVersion spec = do
  oldVersion <- getCurrentVersion
  newVersion <- getNextVersion spec oldVersion
  Bower.writeBowerJson
  Git.addAllChanges
  tagNewVersion oldVersion newVersion
