{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Templates where

import Data.FileEmbed
import System.FilePath ((</>))
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text as T
import Data.Aeson.Encode.Pretty
import qualified Types

packagesDhall :: T.Text
packagesDhall = $(embedStringFile "templates/packages.dhall")

spacchettiDhall :: T.Text
spacchettiDhall = $(embedStringFile "templates/spacchetti.dhall")

srcMain :: T.Text
srcMain = $(embedStringFile "templates/srcMain.purs")

testMain :: T.Text
testMain = $(embedStringFile "templates/testMain.purs")

gitignore :: T.Text
gitignore = $(embedStringFile "templates/gitignore")

encodePscPackage :: Types.PscPackage -> T.Text
encodePscPackage = LT.toStrict . LT.decodeUtf8 . encodePretty

pscPackageJson :: T.Text -> T.Text
pscPackageJson name = encodePscPackage $ Types.PscPackage name "local" "" []
