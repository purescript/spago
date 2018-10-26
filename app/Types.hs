{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import GHC.Generics
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.TH

data PscPackage = PscPackage { name :: T.Text, set :: T.Text, source :: T.Text, depends :: [T.Text] }
  deriving (Show, Generic)

$(deriveJSON defaultOptions ''PscPackage)
