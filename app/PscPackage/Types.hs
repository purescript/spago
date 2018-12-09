{-# LANGUAGE TemplateHaskell #-}

module PscPackage.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text     (Text)
import           GHC.Generics

data PscPackage = PscPackage
  { name    :: Text
  , set     :: Text
  , source  :: Text
  , depends :: [Text]
  }
  deriving (Show, Generic)

$(deriveJSON defaultOptions ''PscPackage)
