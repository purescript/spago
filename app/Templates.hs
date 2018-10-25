{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Templates where

import GHC.Generics
import Data.FileEmbed
import System.FilePath ((</>))
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty

data PscPackage = PscPackage { name :: T.Text
                             , set :: T.Text
                             , source :: T.Text
                             , depends :: [T.Text]
                             }
                  deriving (Show, Generic)
$(deriveJSON defaultOptions ''PscPackage)

packagesDhall :: T.Text
packagesDhall = $(embedStringFile "templates/packages.dhall")

encodePscPackage :: PscPackage -> T.Text
encodePscPackage = LT.toStrict . LT.decodeUtf8 . encodePretty

pscPackageJson :: T.Text -> T.Text
pscPackageJson name = encodePscPackage $ PscPackage name "local" "" []
