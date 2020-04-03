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
  , WithSrcMap(..)
  , DocsFormat(..)
  ) where

import           Spago.Prelude

import qualified Data.Text      as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Versions  as Version
import qualified Spago.Dhall    as Dhall

import qualified Spago.Messages as Messages
import qualified Turtle.Bytes

newtype ModuleName = ModuleName { unModuleName :: Text }
newtype TargetPath = TargetPath { unTargetPath :: Text }
newtype SourcePath = SourcePath { unSourcePath :: Text }
  deriving newtype (Show, Dhall.FromDhall)
newtype ExtraArg = ExtraArg { unExtraArg :: Text }
  deriving newtype (Eq)

data WithMain = WithMain | WithoutMain

data WithSrcMap = WithSrcMap | WithoutSrcMap

compile 
  :: (HasPurs env, HasLogFunc env) 
  => [SourcePath] -> [ExtraArg] 
  -> RIO env ()
compile sourcePaths extraArgs = do
  purs <- view pursL
  logDebug $ "Compiling with " <> displayShow purs
  let
    paths = Text.intercalate " " $ surroundQuote <$> map unSourcePath sourcePaths
    args  = Text.intercalate " " $ map unExtraArg extraArgs
    cmd = purs <> " compile " <> args <> " " <> paths
  runWithOutput cmd
    "Build succeeded."
    "Failed to build."

repl :: [SourcePath] -> [ExtraArg] -> IO ()
repl sourcePaths extraArgs = do
  let paths = Text.intercalate " " $ surroundQuote <$> map unSourcePath sourcePaths
      args = Text.intercalate " " $ map unExtraArg extraArgs
      cmd = "purs repl " <> paths <> " " <> args

  viewShell $ callCommand $ Text.unpack cmd


bundle :: HasLogFunc env => WithMain -> WithSrcMap -> ModuleName -> TargetPath -> RIO env ()
bundle withMain withSourceMap (ModuleName moduleName) (TargetPath targetPath) = do
  let main = case withMain of
        WithMain    -> " --main " <> moduleName
        WithoutMain -> ""

      sourceMap = case withSourceMap of 
        WithSrcMap    -> " --source-maps"
        WithoutSrcMap -> ""

      cmd
        = "purs bundle \"output/*/*.js\""
        <> " -m " <> moduleName
        <> main
        <> " -o " <> targetPath
        <> sourceMap

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


docs :: HasLogFunc env => DocsFormat -> [SourcePath] -> RIO env ()
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

version :: RIO env (Either Text Version.SemVer)
version = Turtle.Bytes.shellStrictWithErr (purs <> " --version") empty >>= \case
  (ExitSuccess, out, _err) -> do
    let versionText = headMay $ Text.split (== ' ') (Text.Encoding.decodeUtf8With lenientDecode out)
        parsed = versionText >>= (hush . Version.semver)

    pure $ case parsed of
      Nothing -> Left $ Messages.failedToParseCommandOutput (purs <> " --version") (Text.Encoding.decodeUtf8With lenientDecode out)
      Just p -> Right p
  (_, _out, _err) -> pure $ Left $ "Failed to run '" <> purs <> " --version'"
  where
    purs = "purs"


runWithOutput :: HasLogFunc env => Text -> Text -> Text -> RIO env ()
runWithOutput command success failure = do
  logDebug $ "Running command: `" <> display command <> "`"
  shell command empty >>= \case
    ExitSuccess -> logInfo $ display success
    ExitFailure _ -> die [ display failure ]
