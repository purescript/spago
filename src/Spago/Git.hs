module Spago.Git
  ( requireCleanWorkingTree
  , hasCleanWorkingTree
  , getAllTags
  , commitAndTag
  , isIgnored
  ) where

import           Spago.Prelude

import qualified Data.Text     as Text
import qualified Turtle


requireCleanWorkingTree :: Spago m => m ()
requireCleanWorkingTree = do
  clean <- hasCleanWorkingTree
  unless clean $ do
    die "Your git working tree is dirty. Please commit or stash your changes first."


hasCleanWorkingTree :: Spago m => m Bool
hasCleanWorkingTree = do
  (code, stdout, stderr) <- Turtle.procStrictWithErr "git" ["status", "--porcelain"] empty

  when (code /= ExitSuccess) $ do
    echoDebug $ "git status stderr: " <> stderr
    die "Unable to check git status. Perhaps git is not installed or this is not a git repository?"

  pure $ stdout == ""


getAllTags :: Spago m => m [Text]
getAllTags = do
  fmap Text.lines $ Turtle.strict $ Turtle.inproc "git" ["tag", "--list"] empty


commitAndTag :: Spago m => Text -> Text -> m ()
commitAndTag tag message = do
  Turtle.procs "git" ["commit", "--quiet", "--allow-empty", "--message=" <> message] empty
  Turtle.procs "git" ["tag", "--annotate", "--message=" <> message, tag] empty


isIgnored :: Spago m => Text -> m Bool
isIgnored path = do
  (== ExitSuccess) <$> Turtle.proc "git" ["check-ignore", "--quiet", path] empty
