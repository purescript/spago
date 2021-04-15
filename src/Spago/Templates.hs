{-# LANGUAGE TemplateHaskell #-}
module Spago.Templates where

import qualified Data.Text                as T

import           Spago.TH                 (embedFileUtf8)

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

bowerJson :: T.Text
bowerJson = $(embedFileUtf8 "templates/bower.json")

pursRepl :: T.Text
pursRepl = $(embedFileUtf8 "templates/purs-repl")

docsSearchApp0010 :: T.Text
docsSearchApp0010 = $(embedFileUtf8 "templates/docs-search-app-0.0.10.js")

docsSearch0010 :: T.Text
docsSearch0010 = $(embedFileUtf8 "templates/purescript-docs-search-0.0.10")

docsSearchApp0011 :: T.Text
docsSearchApp0011 = $(embedFileUtf8 "templates/docs-search-app-0.0.11.js")

docsSearch0011 :: T.Text
docsSearch0011 = $(embedFileUtf8 "templates/purescript-docs-search-0.0.11")
