module Spago.Cmd (getCmdVersion) where

import qualified Spago.Messages as Messages
import qualified Turtle.Bytes
import           Spago.Prelude
import qualified Data.Text      as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Text.Encoding
import qualified Data.Versions  as Version

-- | Get the semantic version of a command, e.g. purs --version
getCmdVersion :: forall io. MonadIO io => Text -> io (Either Text Version.SemVer)
getCmdVersion cmd =
  Turtle.Bytes.shellStrictWithErr (cmd <> " --version") empty >>= \case
    (ExitSuccess, out, _err) -> do
      let versionText = headMay $ Text.split (== ' ') (Text.strip $ Text.Encoding.decodeUtf8With lenientDecode out)
          parsed = versionText >>= (\vt -> Text.stripPrefix "v" vt <|> Just vt) >>= (hush . Version.semver)

      pure $ case parsed of
        Nothing ->
          Left $
            Messages.failedToParseCommandOutput
              (cmd <> " --version")
              (Text.Encoding.decodeUtf8With Text.Encoding.lenientDecode out)
        Just p -> Right p
    (_, _out, _err) -> pure $ Left $ "Failed to run '" <> cmd <> " --version'"
