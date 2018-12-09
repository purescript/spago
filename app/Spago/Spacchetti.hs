module Spago.Spacchetti where

import           Data.Aeson
import           Data.Map     (Map)
import           Data.Text    (Text)
import qualified Dhall
import           GHC.Generics (Generic)


-- | Matches the packages definition of Spacchetti Package.dhall/psc-package
newtype PackageName = PackageName { packageName :: Text }
  deriving (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Dhall.Interpret)

-- | A spacchetti package.
data Package = Package
  { dependencies :: [PackageName] -- ^ list of dependency package names
  , repo         :: Text          -- ^ the git repository
  , version      :: Text          -- ^ version string (also functions as a git ref)
  }
  deriving (Show, Generic)

instance ToJSON Package
instance FromJSON Package

type Packages = Map PackageName Package

