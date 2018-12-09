{-# LANGUAGE TemplateHaskell #-}

module Spago.Templates where

import           Data.Aeson.Encode.Pretty
import           Data.FileEmbed
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT

import           PscPackage.Types         (PscPackage (..))

packagesDhall :: T.Text
packagesDhall = $(embedStringFile "templates/packages.dhall")

spagoDhall :: T.Text
spagoDhall = $(embedStringFile "templates/spago.dhall")

srcMain :: T.Text
srcMain = $(embedStringFile "templates/srcMain.purs")

testMain :: T.Text
testMain = $(embedStringFile "templates/testMain.purs")

gitignore :: T.Text
gitignore = $(embedStringFile "templates/gitignore")

encodePscPackage :: PscPackage -> T.Text
encodePscPackage = LT.toStrict . LT.decodeUtf8 . encodePretty

pscPackageJson :: T.Text -> T.Text
pscPackageJson packageName = encodePscPackage $ PscPackage packageName "local" "" []
