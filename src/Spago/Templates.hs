{-# LANGUAGE TemplateHaskell #-}
module Spago.Templates where

import qualified Data.ByteString.Internal as B
import           Data.FileEmbed           (embedFile)
import qualified Data.Text                as T

import           Spago.TH                 (embedFileUtf8, embedURLWithFallback)
import           Spago.Prelude


packagesDhall :: T.Text
packagesDhall = $(embedFileUtf8 "templates/packages.dhall")

spagoDhall :: T.Text
spagoDhall = $(embedFileUtf8 "templates/spago.dhall")

srcMain :: T.Text
srcMain = $(embedFileUtf8 "templates/srcMain.purs")

testMain :: T.Text
testMain = $(embedFileUtf8 "templates/testMain.purs")

gitignore :: T.Text
gitignore = $(embedFileUtf8 "templates/gitignore")

bowerJson :: B.ByteString
bowerJson = $(embedFile "templates/bower.json")

docsSearchApp :: T.Text
docsSearchApp =
  $(embedURLWithFallback
    ( "https://github.com/spacchetti/purescript-docs-search/releases/download/"
   <> T.unpack docsSearchVersion
   <> "/docs-search-app.js"
    )
    "templates/docs-search-app.js")

docsSearch :: T.Text
docsSearch =
  $(embedURLWithFallback
     ( "https://github.com/spacchetti/purescript-docs-search/releases/download/"
    <> T.unpack docsSearchVersion
    <> "/purescript-docs-search"
     )
     "templates/purescript-docs-search")
