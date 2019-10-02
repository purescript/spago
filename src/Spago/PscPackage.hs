module Spago.PscPackage where

import           Spago.Prelude hiding (set)


data PscPackage = PscPackage
  { name    :: Text
  , set     :: Text
  , source  :: Text
  , depends :: [Text]
  }
  deriving (Show, Generic)

instance ToJSON PscPackage
instance FromJSON PscPackage

configPath :: IsString t => t
configPath = "psc-package.json"

