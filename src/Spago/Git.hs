module Spago.Git
  ( requireCleanWorkingTree
  , getAllTags
  , addAllChanges
  , commitAndTag
  , isIgnored
  ) where

import           Spago.Prelude

import qualified Data.Text     as Text
import qualified Turtle


requireCleanWorkingTree :: Spago m => m ()
requireCleanWorkingTree = do
  (code, stdout, stderr) <- Turtle.procStrictWithErr "git" ["status", "--porcelain"] empty

  when (code /= ExitSuccess) $ do
    echoDebug $ "git status stderr: " <> stderr
    die "Unable to check git status. Perhaps git is not installed or this is not a git repository?"

  when (stdout /= "") $ do
    die "Your git working tree is dirty. Please commit or stash your changes first."


getAllTags :: Spago m => m [Text]
getAllTags = do
  fmap Text.lines $ Turtle.strict $ Turtle.inproc "git" ["tag", "--list"] empty


addAllChanges :: Spago m => m ()
addAllChanges = do
  Turtle.procs "git" ["add", "-A"] empty


commitAndTag :: Spago m => Text -> Text -> m ()
commitAndTag tag message = do
  Turtle.procs "git" ["commit", "--quiet", "--allow-empty", "--message=" <> message] empty
  Turtle.procs "git" ["tag", "--annotate", "--message=" <> message, tag] empty


isIgnored :: Spago m => Text -> m Bool
isIgnored path = do
  (== ExitSuccess) <$> Turtle.proc "git" ["check-ignore", "--quiet", path] empty
