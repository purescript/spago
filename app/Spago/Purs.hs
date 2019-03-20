module Spago.Purs where

import Spago.Prelude

import qualified Data.Text              as Text
import           Data.Versions          as Version
import qualified System.Process         as Process

import qualified Spago.Messages         as Messages
import qualified Turtle


newtype ModuleName = ModuleName { unModuleName :: Text }
newtype TargetPath = TargetPath { unTargetPath :: Text }
newtype SourcePath = SourcePath { unSourcePath :: Text }
newtype ExtraArg = ExtraArg { unExtraArg :: Text }

data WithMain = WithMain | WithoutMain


compile :: Spago m => [SourcePath] -> [ExtraArg] -> m ()
compile sourcePaths extraArgs = do
  let
    paths = Text.intercalate " " $ Messages.surroundQuote <$> map unSourcePath sourcePaths
    args  = Text.intercalate " " $ map unExtraArg extraArgs
    cmd = "purs compile " <> args <> " " <> paths
  runWithOutput cmd
    "Build succeeded."
    "Failed to build."

repl :: Spago m => [SourcePath] -> [ExtraArg] -> m ()
repl sourcePaths extraArgs = do
  let args  = Text.unpack
        <$> ["repl"]
        <> map unSourcePath sourcePaths
        <> map unExtraArg extraArgs
  Turtle.view $ liftIO $ Process.callProcess "purs" args

bundle :: Spago m => WithMain -> ModuleName -> TargetPath -> m ()
bundle withMain (ModuleName moduleName) (TargetPath targetPath) = do
  let main = case withMain of
        WithMain    -> " --main " <> moduleName
        WithoutMain -> ""

      cmd
        = "purs bundle \"output/*/*.js\""
        <> " -m " <> moduleName
        <> main
        <> " -o " <> targetPath

  runWithOutput cmd
    ("Bundle succeeded and output file to " <> targetPath)
    ("Bundle failed.")


docs :: Spago m => [SourcePath] -> m ()
docs sourcePaths = do
  let
    paths = Text.intercalate " " $ Messages.surroundQuote <$> map unSourcePath sourcePaths
    cmd = "purs docs " <> paths <> " --format html"
  runWithOutput cmd
    ("Docs generated. Index is at " <> Messages.surroundQuote "./generated-docs/index.html")
    "Docs generation failed."

version :: Spago m => m (Maybe Version.SemVer)
version = do
  fullVersionText <- liftIO $ Turtle.shellStrict "purs --version" Turtle.empty >>= \case
    (Turtle.ExitSuccess, out) -> pure out
    _ -> die "Failed to run 'purs --version'"
  versionText <- pure $ headMay $ Text.split (== ' ') fullVersionText
  parsed <- pure $ versionText >>= (hush . Version.semver)

  when (isNothing parsed) $ do
    echo $ Messages.failedToParseCommandOutput "purs --version" fullVersionText

  pure parsed


runWithOutput :: Spago m => Text -> Text -> Text -> m ()
runWithOutput command success failure = do
  echoDebug $ "Running command: `" <> command <> "`"
  liftIO $ Turtle.shell command Turtle.empty >>= \case
    Turtle.ExitSuccess -> echo success
    Turtle.ExitFailure _ -> die failure
