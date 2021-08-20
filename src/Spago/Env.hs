module Spago.Env
  (
  -- | Environments
    GlobalOptions(..)
  , Env(..)
  , PackageSetEnv(..)
  , ReplEnv(..)
  , InstallEnv(..)
  , PublishEnv(..)
  , VerifyEnv(..)
  , BuildEnv(..)
  , PursEnv(..)

  -- | Environment constraints
  , HasEnv
  , HasVerifyEnv
  , HasPublishEnv
  , HasBuildEnv
  , HasPursEnv

  -- | Simple capabilities
  , HasGlobalCache
  , HasConfigPath
  , HasJobs
  , HasPackageSet
  , HasConfig
  , HasGit
  , HasBower
  , HasTarget
  , HasTargetName
  , HasPurs

  -- | Other types
  , module Spago.Types
  ) where

import RIO (LogFunc, Generic, Maybe, Text, Bool, Int)

import Data.Generics.Product (HasType)

import Spago.Types


data GlobalOptions = GlobalOptions
  { globalQuiet       :: Bool
  , globalVerbose     :: Bool
  , globalVeryVerbose :: Bool
  , globalUseColor    :: Bool
  , globalUsePsa      :: UsePsa
  , globalJobs        :: Maybe Int
  , globalConfigPath  :: Maybe Text
  , globalCacheConfig :: Maybe CacheFlag
  }

type HasLogFunc env = HasType LogFunc env
type HasJobs env = HasType Jobs env
type HasGlobalCache env = HasType GlobalCache env
type HasConfigPath env = HasType ConfigPath env
type HasPackageSet env = HasType PackageSet env
type HasPurs env = HasType PursCmd env
type HasGit env = HasType GitCmd env
type HasBower env = HasType BowerCmd env
type HasTarget env = HasType Target env
type HasTargetName env = HasType TargetName env

type HasEnv env =
  ( HasLogFunc env
  , HasJobs env
  , HasConfigPath env
  , HasGlobalCache env
  )

type HasConfig env = ( HasType Config env, HasPackageSet env )
type HasMaybeConfig env = ( HasType (Maybe Config) env, HasPackageSet env )
type HasMaybeGraph env = HasType (Maybe ModuleGraph) env
type HasBuildOptions env = HasType BuildOptions env

type HasVerifyEnv env =
  ( HasLogFunc env
  , HasJobs env
  , HasGlobalCache env
  , HasPurs env
  , HasPackageSet env
  , HasMaybeConfig env
  )

type HasPublishEnv env =
  ( HasLogFunc env
  , HasJobs env
  , HasConfig env
  , HasBower env
  , HasGit env
  )

type HasBuildEnv env =
  ( HasEnv env
  , HasPurs env
  , HasGit env
  , HasConfig env
  , HasMaybeGraph env
  , HasBuildOptions env
  , HasTarget env
  , HasTargetName env
  )

type HasPursEnv env =
  ( HasEnv env
  , HasPurs env
  )

-- | App configuration containing parameters and other common
--   things it's useful to compute only once at startup.
data Env = Env
  { envLogFunc :: !LogFunc
  , envJobs :: !Jobs
  , envConfigPath :: !ConfigPath
  , envGlobalCache :: !GlobalCache
  } deriving (Generic)

data PackageSetEnv = PackageSetEnv
  { envLogFunc :: !LogFunc
  , envPackageSet :: !PackageSet
  } deriving (Generic)

data VerifyEnv = VerifyEnv
  { envLogFunc :: !LogFunc
  , envJobs :: !Jobs
  , envGlobalCache :: !GlobalCache
  , envPursCmd :: !PursCmd
  , envPackageSet :: !PackageSet
  , envConfig :: !(Maybe Config)
  } deriving (Generic)

data ReplEnv = ReplEnv
  { envLogFunc :: !LogFunc
  , envJobs :: !Jobs
  , envConfigPath :: !ConfigPath
  , envGlobalCache :: !GlobalCache
  , envPackageSet :: !PackageSet
  , envTarget :: !Target
  } deriving (Generic)

data InstallEnv = InstallEnv
  { envLogFunc :: !LogFunc
  , envJobs :: !Jobs
  , envConfigPath :: !ConfigPath
  , envGlobalCache :: !GlobalCache
  , envPackageSet :: !PackageSet
  , envConfig :: !Config
  , envTarget :: !Target
  , envTargetName :: !TargetName
  } deriving (Generic)

data PublishEnv = PublishEnv
  { envLogFunc :: !LogFunc
  , envJobs :: !Jobs
  , envConfig :: !Config
  , envPackageSet :: !PackageSet
  , envGitCmd :: !GitCmd
  , envBowerCmd :: !BowerCmd
  , envTarget :: !Target
  , envTargetName :: !TargetName
  } deriving (Generic)

data BuildEnv = BuildEnv
  { envLogFunc :: !LogFunc
  , envJobs :: !Jobs
  , envConfigPath :: !ConfigPath
  , envGlobalCache :: !GlobalCache
  , envPursCmd :: !PursCmd
  , envGitCmd :: !GitCmd
  , envPackageSet :: !PackageSet
  , envConfig :: !Config
  , envGraph :: !(Maybe ModuleGraph)
  , envBuildOptions :: !BuildOptions
  , envTarget :: !Target
  , envTargetName :: !TargetName
  } deriving (Generic)

data PursEnv = PursEnv
  { envLogFunc :: !LogFunc
  , envJobs :: !Jobs
  , envConfigPath :: !ConfigPath
  , envGlobalCache :: !GlobalCache
  , envPursCmd :: !PursCmd
  } deriving (Generic)
