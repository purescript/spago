module Spago.Spacchetti where

import           Data.Aeson
import           Data.Map     (Map)
import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Dhall
import           GHC.Generics (Generic)
import           Network.URI  (parseURI)


-- | Matches the packages definition of Spacchetti Package.dhall/psc-package
newtype PackageName = PackageName { packageName :: Text }
  deriving (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Dhall.Interpret)

-- | A spacchetti package.
data Package = Package
  { dependencies :: [PackageName] -- ^ list of dependency package names
  , repo         :: Repo          -- ^ the git repository
  , version      :: Text          -- ^ version string (also functions as a git ref)
  }
  deriving (Show, Generic)

instance ToJSON Package
instance FromJSON Package

type Packages = Map PackageName Package

data Repo
  = Local Text
  | Remote Text
  deriving (Show, Generic)

instance ToJSON Repo
instance FromJSON Repo

instance Dhall.Interpret Repo where
  autoWith _ = makeRepo <$> Dhall.strictText
    where
      makeRepo path = case parseURI $ Text.unpack path of
        Just _uri -> Remote path
        Nothing   -> Local path
