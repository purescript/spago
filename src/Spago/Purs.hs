module Spago.Purs
  ( compile
  , repl
  , bundle
  , docs
  , version
  , parseDocsFormat
  , SourcePath(..)
  , TargetPath(..)
  , ModuleName(..)
  , ExtraArg(..)
  , WithMain(..)
  , DocsFormat(..)
  ) where

import           Spago.Prelude

import qualified Data.Text      as Text
import qualified Data.Versions  as Version
import qualified Spago.Dhall    as Dhall

import qualified Spago.Messages as Messages


newtype ModuleName = ModuleName { unModuleName :: Text }
newtype TargetPath = TargetPath { unTargetPath :: Text }
newtype SourcePath = SourcePath { unSourcePath :: Text }
  deriving newtype (Show, Dhall.Interpret)
newtype ExtraArg = ExtraArg { unExtraArg :: Text }
  deriving newtype (Eq)

data WithMain = WithMain | WithoutMain


compile :: Spago m => [SourcePath] -> [ExtraArg] -> m ()
compile sourcePaths extraArgs = do
  -- first we decide if we _want_ to use psa, then if we _can_
  usePsa <- asks globalUsePsa
  purs <- case usePsa of
    NoPsa -> pure "purs"
    UsePsa -> try psaVersion >>= \case
      Right _ -> pure "psa"
      Left (_err :: SomeException) -> pure "purs"

  echoDebug $ "Compiling with " <> surroundQuote purs

  let
    paths = Text.intercalate " " $ surroundQuote <$> map unSourcePath sourcePaths
    args  = Text.intercalate " " $ map unExtraArg extraArgs
    cmd = purs <> " compile " <> args <> " " <> paths
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
    "Bundle failed."


data DocsFormat
  = Html
  | Markdown
  | Ctags
  | Etags
  deriving (Eq)

parseDocsFormat :: Text -> Maybe DocsFormat
parseDocsFormat = \case
  "html"     -> Just Html
  "markdown" -> Just Markdown
  "ctags"    -> Just Ctags
  "etags"    -> Just Etags
  _          -> Nothing


docs :: Spago m => DocsFormat -> [SourcePath] -> m ()
docs format sourcePaths = do
  let
    printDocsFormat :: DocsFormat -> Text
    printDocsFormat = \case
      Html     -> "html"
      Markdown -> "markdown"
      Ctags    -> "ctags"
      Etags    -> "etags"

    paths = Text.intercalate " " $ surroundQuote <$> map unSourcePath sourcePaths
    formatStr = printDocsFormat format
    cmd = "purs docs " <> paths <> " --format " <> formatStr
  runWithOutput cmd
    "Docs generation succeeded."
    "Docs generation failed."

version, psaVersion :: Spago m => m (Maybe Version.SemVer)
version = versionImpl "purs"
psaVersion = versionImpl "psa"

versionImpl :: Spago m => Text -> m (Maybe Version.SemVer)
versionImpl purs = do
  fullVersionText <- shellStrictWithErr (purs <> " --version") empty >>= \case
    (ExitSuccess, out, _err) -> pure out
    (_, _out, err) -> die $ "Failed to run '" <> purs <> " --version'. Error:" <> err
  let versionText = headMay $ Text.split (== ' ') fullVersionText
      parsed = versionText >>= (hush . Version.semver)

  when (isNothing parsed) $ do
    echo $ Messages.failedToParseCommandOutput (purs <> " --version") fullVersionText

  pure parsed


runWithOutput :: Spago m => Text -> Text -> Text -> m ()
runWithOutput command success failure = do
  echoDebug $ "Running command: `" <> command <> "`"
  liftIO $ shell command empty >>= \case
    ExitSuccess -> echo success
    ExitFailure _ -> die failure
