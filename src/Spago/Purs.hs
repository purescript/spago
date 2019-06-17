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
  purs <- asks optionsPursCommand
  let
    paths = Text.intercalate " " $ Messages.surroundQuote <$> map unSourcePath sourcePaths
    args  = Text.intercalate " " $ map unExtraArg extraArgs
    cmd = purs <> " compile " <> args <> " " <> paths
  runWithOutput cmd
    "Build succeeded."
    "Failed to build."

repl :: Spago m => [SourcePath] -> [ExtraArg] -> m ()
repl sourcePaths extraArgs = do
  purs <- asks optionsPursCommand
  let paths = Text.intercalate " " $ Messages.surroundQuote <$> map unSourcePath sourcePaths
      args = Text.intercalate " " $ map unExtraArg extraArgs
      cmd = purs <> " repl " <> paths <> " " <> args

  viewShell $ callCommand $ Text.unpack cmd


bundle :: Spago m => WithMain -> ModuleName -> TargetPath -> m ()
bundle withMain (ModuleName moduleName) (TargetPath targetPath) = do
  purs <- asks optionsPursCommand
  let main = case withMain of
        WithMain    -> " --main " <> moduleName
        WithoutMain -> ""

      cmd
        = purs <> " bundle \"output/*/*.js\""
        <> " -m " <> moduleName
        <> main
        <> " -o " <> targetPath

  runWithOutput cmd
    ("Bundle succeeded and output file to " <> targetPath)
    ("Bundle failed.")


docs :: Spago m => [SourcePath] -> m ()
docs sourcePaths = do
  purs <- asks optionsPursCommand
  let
    paths = Text.intercalate " " $ Messages.surroundQuote <$> map unSourcePath sourcePaths
    cmd = purs <> " docs " <> paths <> " --format html"
  runWithOutput cmd
    ("Docs generated. Index is at " <> Messages.surroundQuote "./generated-docs/index.html")
    "Docs generation failed."

version :: Spago m => m (Maybe Version.SemVer)
version = do
  purs <- asks optionsPursCommand
  let cmd = purs <> " --version"
  echoDebug $ "Running command: `" <> cmd <> "`"
  fullVersionText <- shellStrict cmd empty >>= \case
    (ExitSuccess, out) -> pure out
    _ -> die $ "Failed to run '" <> cmd <> "'"
  versionText <- pure $ headMay $ Text.split (== ' ') fullVersionText
  parsed <- pure $ versionText >>= (hush . Version.semver)

  when (isNothing parsed) $ do
    echo $ Messages.failedToParseCommandOutput cmd fullVersionText

  pure parsed


runWithOutput :: Spago m => Text -> Text -> Text -> m ()
runWithOutput command success failure = do
  echoDebug $ "Running command: `" <> command <> "`"
  liftIO $ shell command empty >>= \case
    ExitSuccess -> echo success
    ExitFailure _ -> die failure
