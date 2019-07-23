{-# LANGUAGE TemplateHaskell #-}

module Spago.TH
  ( embedFileUtf8
  , embedURLWithFallback
  ) where

import           Prelude

import           Control.Exception           (try, SomeException)
import           Data.ByteString             (ByteString)
import           Data.FileEmbed
import           Data.Text                   (unpack)
import           Data.Text.Encoding          as LT
import           Language.Haskell.TH.Syntax  (Exp(..), Q, runIO, Lit(StringL))
import           Network.HTTP.Client.Conduit (parseUrlThrow)
import           Network.HTTP.Simple         (httpBS, getResponseBody)
import qualified Data.ByteString             as BS

-- | This is here so that we can embed files as Utf8 Text.
--   The reason for that is that since we have unicode Dhall files,
--   if you compile on a non-unicode system you'll get weirdly encoded stuff.
--
--   TL;DR: don't use embedStringFile.
--
--   This comes from:
--   https://github.com/snoyberg/file-embed/issues/27#issuecomment-411694346
embedFileUtf8 :: FilePath -> Q Exp
embedFileUtf8 filePath =
  [| LT.decodeUtf8 $(makeRelativeToProject filePath >>= embedFile) |]

-- | Embed a file from a URL or use a local file as a fallback.
--   If fetching the URL was successfull, write the contents to the fallback file.
embedURLWithFallback
  :: String
  -- ^ A URL.
  -> FilePath
  -- ^ A fallback file.
  -> Q Exp
embedURLWithFallback url filePath = do

  eiResponseBody <- runIO $ try $ do
    req <- parseUrlThrow url
    getResponseBody <$> httpBS req

  case eiResponseBody :: Either SomeException ByteString of
    Left _ -> embedFileUtf8 filePath
    Right responseBody -> do
      runIO $ BS.writeFile filePath responseBody
      return . LitE . StringL . unpack $ LT.decodeUtf8 responseBody
