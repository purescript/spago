{-# LANGUAGE TemplateHaskell #-}

module Spago.Templates where

import           Data.Aeson.Encode.Pretty
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT

import           PscPackage.Types         (PscPackage (..))
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

encodePscPackage :: PscPackage -> T.Text
encodePscPackage = LT.toStrict . LT.decodeUtf8 . encodePretty

pscPackageJson :: T.Text -> T.Text
pscPackageJson packageName = encodePscPackage $ PscPackage packageName "local" "" []
