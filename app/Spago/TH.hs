{-# LANGUAGE TemplateHaskell #-}

module Spago.TH
  ( embedFileUtf8
  ) where

import           Data.FileEmbed
import           Data.Text.Encoding         as LT
import           Language.Haskell.TH.Syntax (Exp, Q)
import           Prelude


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
