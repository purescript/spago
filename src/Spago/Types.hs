module Spago.Types where

import           Spago.Prelude

import qualified Data.Text      as Text
import qualified Data.Versions  as Version
import qualified Dhall
import qualified Network.URI    as URI

import qualified Spago.Messages as Messages


newtype PackageName = PackageName { packageName :: Text }
  deriving (Show, Read, Data)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Dhall.FromDhall)

-- | A package-set package.
--   Matches the packages definition in Package.dhall from package-sets
data Package = Package
  { dependencies :: ![PackageName]   -- ^ list of dependency package names
  , location     :: !PackageLocation -- ^ info about where the package is located
  }
  deriving (Eq, Show, Generic)


data PackageLocation
  = Remote
      { repo    :: !Repo          -- ^ the remote git repository
      , version :: !Text          -- ^ version string (also functions as a git ref)
      }
  | Local
      { localPath :: !Text        -- ^ local path of the package
      }
  deriving (Eq, Show, Generic)


-- | This instance is to make `spago ls packages --json` work
instance ToJSON PackageLocation where
  toJSON Remote{..} = object
    [ "tag" .= ("Remote" :: Text)
    , "contents" .= unRepo repo
    ]
  toJSON Local{..} = object
    [ "tag" .= ("Local" :: Text)
    , "contents" .= localPath
    ]

data PackageSet = PackageSet
  { packagesDB             :: Map PackageName Package
  , packagesMinPursVersion :: Maybe Version.SemVer
  }
  deriving (Show, Generic)


-- | We consider a "Repo" a "box of source to include in the build"
--   This can have different nature:
newtype Repo = Repo { unRepo :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON Repo

instance Dhall.FromDhall Repo where
  autoWith _ = makeRepo <$> Dhall.strictText
    where
      -- We consider a "Remote" anything that `parseURI` thinks is a URI
      makeRepo repo = case URI.parseURI $ Text.unpack repo of
        Just _uri -> Repo repo
        Nothing   -> error $ Text.unpack $ Messages.failedToParseRepoString repo


-- | Whether to force an action
data Force = Force | NoForce
  deriving (Eq)