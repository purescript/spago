module Spago.Dhall where

import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as Text
import qualified Dhall.JSON               as Dhall.JSON
import qualified Turtle                   as T


unsafePathToText :: T.FilePath -> Text
unsafePathToText p = case T.toText p of
  Left t  -> t
  Right t -> t


-- | Given a path to a Dhall file and an output path to a JSON file,
--   reads the Dhall, converts it, and writes it as JSON
dhallToJSON :: T.FilePath -> T.FilePath -> IO ()
dhallToJSON inputPath outputPath = do
  let config = JSON.Config
               { JSON.confIndent = JSON.Spaces 2
               , JSON.confCompare = compare
               , JSON.confNumFormat = JSON.Generic
               , JSON.confTrailingNewline = False }

  dhall <- T.readTextFile inputPath

  json <- Dhall.JSON.codeToValue Dhall.JSON.NoConversion (unsafePathToText inputPath) dhall

  T.writeTextFile outputPath
    $ Text.decodeUtf8
    $ ByteString.Lazy.toStrict
    $ JSON.encodePretty' config json
