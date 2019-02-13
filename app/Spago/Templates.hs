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
