{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import GHC.Generics
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as Map

data PscPackage = PscPackage { name :: T.Text, set :: T.Text, source :: T.Text, depends :: [T.Text] }
  deriving (Show, Generic)

$(deriveJSON defaultOptions ''PscPackage)

-- | Matches the packages definition of Spacchetti Package.dhall/psc-package
newtype PackageName = PackageName { unwrap :: T.Text }
  deriving (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data PackageDefinition = PackageDefinition
  { repo :: T.Text
  , version :: T.Text
  , dependencies :: [PackageName]
  }
  deriving (Show, Generic)

$(deriveJSON defaultOptions ''PackageDefinition)

type Packages = Map.Map PackageName PackageDefinition

-- | Spacchetti configuration file JSON type
data SpacchettiConfig = SpacchettiConfig
  { name :: T.Text
  , dependencies :: [PackageName]
  , packages :: Packages
  }

$(deriveJSON defaultOptions ''SpacchettiConfig)
