{-# LANGUAGE DeriveAnyClass #-}
module Spago.Env
  (
  -- | Environments
    GlobalOptions(..)
  , Env(..)
  , PackageSetEnv(..)
  , InstallEnv(..)
  , PublishEnv(..)
  , VerifyEnv(..)
  , BuildEnv(..)

  -- | AAA
  , HasInstallEnv

  -- | TODO
  , HasGlobalCache
  , HasConfigPath
  , HasJobs
  , HasPackageSet
  , HasConfig
  , HasGit
  , HasBower
  , HasPurs

  -- | Other types
  , module Spago.Types
  ) where

import RIO

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

type HasJobs env = HasType Jobs env
type HasGlobalCache env = HasType GlobalCache env
type HasConfigPath env = HasType ConfigPath env
type HasPackageSet env = HasType PackageSet env
type HasPurs env = HasType PursCmd env
type HasGit env = HasType GitCmd env
type HasBower env = HasType BowerCmd env

type HasEnv env =
  ( HasLogFunc env
  , HasJobs env
  , HasConfigPath env
  , HasGlobalCache env
  )

type HasConfig env = ( HasType Config env, HasPackageSet env )

type HasInstallEnv env = forall env. ( HasLogFunc env
  , HasConfig env
  , HasLogFunc env
  , HasJobs env
  , HasConfigPath env
  , HasGlobalCache env
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
  , envPursCmd :: !PursCmd
  , envPackageSet :: !PackageSet
  } deriving (Generic)

data InstallEnv = InstallEnv
  { envLogFunc :: !LogFunc
  , envSpagoConfig :: !Config
  } deriving (Generic)

data PublishEnv = PublishEnv
  { envLogFunc :: !LogFunc
  , envGitCmd :: !GitCmd
  -- ^ git command to use
  , envBowerCmd :: !BowerCmd
  -- ^ bower command to use
  } deriving (Generic)

data BuildEnv = BuildEnv
  { envLogFunc :: !LogFunc
  , envPursCmd :: !PursCmd
  } deriving (Generic)