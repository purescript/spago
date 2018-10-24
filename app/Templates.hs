{-# LANGUAGE TemplateHaskell #-}

module Templates where

import Data.FileEmbed
import System.FilePath ((</>))

packagesDhall :: String
packagesDhall = $(embedStringFile "templates/packages.dhall")

pscPackageJson :: String
pscPackageJson = $(embedStringFile "templates/psc-package.json")
