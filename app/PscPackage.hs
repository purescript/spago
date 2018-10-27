{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PscPackage where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.TH

data PscPackage = PscPackage
  { name    :: Text
  , set     :: Text
  , source  :: Text
  , depends :: [Text]
  }
  deriving (Show, Generic)

$(deriveJSON defaultOptions ''PscPackage)
