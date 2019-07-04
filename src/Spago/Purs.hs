module Spago.Purs where

import           Spago.Prelude

import qualified Data.Text      as Text
import           Data.Versions  as Version
import qualified Spago.Dhall    as Dhall

import qualified Spago.Messages as Messages


newtype ModuleName = ModuleName { unModuleName :: Text }
newtype TargetPath = TargetPath { unTargetPath :: Text }
newtype SourcePath = SourcePath { unSourcePath :: Text }
  deriving newtype (Show, Dhall.Interpret)
newtype ExtraArg = ExtraArg { unExtraArg :: Text }

data WithMain = WithMain | WithoutMain


compile :: Spago m => [SourcePath] -> [ExtraArg] -> m ()
compile sourcePaths extraArgs = do
  let
    paths = Text.intercalate " " $ surroundQuote <$> map unSourcePath sourcePaths
    args  = Text.intercalate " " $ map unExtraArg extraArgs
    cmd = "purs compile " <> args <> " " <> paths
  runWithOutput cmd
    "Build succeeded."
    "Failed to build."

repl :: Spago m => [SourcePath] -> [ExtraArg] -> m ()
repl sourcePaths extraArgs = do
  let paths = Text.intercalate " " $ surroundQuote <$> map unSourcePath sourcePaths
      args = Text.intercalate " " $ map unExtraArg extraArgs
      cmd = "purs repl " <> paths <> " " <> args

  viewShell $ callCommand $ Text.unpack cmd


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


data DocsFormat
  = Html
  | Markdown
  | Ctags
  | Etags

parseDocsFormat :: Text -> Maybe DocsFormat
parseDocsFormat = \case
  "html"     -> Just Html
  "markdown" -> Just Markdown
  "ctags"    -> Just Ctags
  "etags"    -> Just Etags
  _          -> Nothing

printDocsFormat :: DocsFormat -> Text
printDocsFormat = \case
  Html     -> "html"
  Markdown -> "markdown"
  Ctags    -> "ctags"
  Etags    -> "etags"


docs :: Spago m => Maybe DocsFormat -> [SourcePath] -> m ()
docs format sourcePaths = do
  let
    paths = Text.intercalate " " $ surroundQuote <$> map unSourcePath sourcePaths
    formatStr = printDocsFormat $ fromMaybe Html format
    cmd = "purs docs " <> paths <> " --format " <> formatStr
  runWithOutput cmd
    "Docs generation succeeded."
    "Docs generation failed."

version :: Spago m => m (Maybe Version.SemVer)
version = do
  fullVersionText <- shellStrict "purs --version" empty >>= \case
    (ExitSuccess, out) -> pure out
    _ -> die "Failed to run 'purs --version'"
  versionText <- pure $ headMay $ Text.split (== ' ') fullVersionText
  parsed <- pure $ versionText >>= (hush . Version.semver)

  when (isNothing parsed) $ do
    echo $ Messages.failedToParseCommandOutput "purs --version" fullVersionText

  pure parsed


runWithOutput :: Spago m => Text -> Text -> Text -> m ()
runWithOutput command success failure = do
  echoDebug $ "Running command: `" <> command <> "`"
  liftIO $ shell command empty >>= \case
    ExitSuccess -> echo success
    ExitFailure _ -> die failure
