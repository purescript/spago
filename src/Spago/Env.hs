module Spago.Env
  ( 
  -- | Global environment
    Env(..)
  , HasEnv(..)
  , HasGlobalCache(..)
  , HasConfigPath(..)
  , HasJobs(..)

  , GlobalOptions(..)

  -- | Package set environment
  , PackageSetEnv(..)
  , HasPackageSetEnv
  , HasPackageSet(..)

  -- | Install environment
  , InstallEnv(..)
  , HasInstallEnv(..)
  , HasConfig(..)
  , HasCacheConfig(..)

  -- | Publish environment
  , PublishEnv(..)
  , HasPublishEnv
  , HasGit(..)
  , HasBower(..)

  -- | Verification environment
  , VerifyEnv(..)
  , HasVerifyEnv

  -- | Build environment
  , BuildEnv(..)
  , HasBuildEnv
  , HasPurs(..)

  -- | Other types
  , module Spago.Types
  ) where

import RIO

import qualified GHC.IO

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


-- | App configuration containing parameters and other common
--   things it's useful to compute only once at startup.
data Env = Env
  { envJobs        :: !Int
  , envConfigPath  :: !Text
  , envLogFunc     :: !LogFunc
  , envGlobalCache :: !GHC.IO.FilePath
  , envCacheConfig :: !(Maybe CacheFlag)
  }

data PackageSetEnv = PackageSetEnv
  { packageSetEnvGlobal :: !Env 
  -- ^ the global options
  , packageSetSet :: !PackageSet
  -- ^ the package set
  }

data VerifyEnv = VerifyEnv
  { verifyEnvGlobal :: !Env 
  -- ^ the global options
  , verifyEnvPurs :: !Text
  -- ^ the purs command to use
  , verifyPackageSet :: PackageSet
  -- ^ the package set
  }

data InstallEnv = InstallEnv
  { installEnvGlobal :: !Env
  -- ^ the global options
  , installSpagoConfig :: !Config
  -- ^ the project config
  }

data PublishEnv = PublishEnv
  { publishEnvInstall :: !InstallEnv
  -- ^ the install config
  , publishEnvGit :: !Text
  -- ^ git command to use
  , publishEnvBower :: !Text
  -- ^ bower command to use
  }

data BuildEnv = BuildEnv 
  { buildInstallEnv :: !InstallEnv 
  -- ^ the install config
  , buildEnvPurs :: !Text 
  -- ^ the purs command to use
  }

-- ### Classes

class HasGlobalCache env where 
  globalCacheL :: Lens' env GHC.IO.FilePath 

class HasConfigPath env where 
  configPathL :: Lens' env Text

class HasJobs env where 
  jobsL :: Lens' env Int

class HasGlobalCache env => HasCacheConfig env where 
  cacheConfigL :: Lens' env (Maybe CacheFlag)

class HasPackageSet env where
  packageSetL :: Lens' env PackageSet

class 
  ( HasPackageSet env
  ) => HasConfig env where
  configL :: Lens' env Config

class HasPurs env where
  pursL :: Lens' env Text

class HasGit env where
  gitL :: Lens' env Text

class HasBower env where
  bowerL :: Lens' env Text


class
  ( HasGlobalCache env
  , HasLogFunc env
  , HasConfigPath env
  , HasJobs env
  ) => HasEnv env where
  envL :: Lens' env Env

class
  ( HasEnv env
  , HasPackageSet env
  ) => HasPackageSetEnv env

class
  ( HasEnv env 
  , HasCacheConfig env
  , HasConfig env
  ) => HasInstallEnv env where
  installEnvL :: Lens' env InstallEnv

class
  ( HasInstallEnv env 
  , HasGit env 
  , HasBower env
  ) => HasPublishEnv env

class
  ( HasPurs env
  , HasInstallEnv env
  ) => HasBuildEnv env

class
  ( HasEnv env
  , HasPurs env
  , HasCacheConfig env
  , HasPackageSet env
  ) => HasVerifyEnv env


-- ### Instances

-- Config
instance HasPackageSet Config where
  packageSetL = lens packageSet (\x y -> x { packageSet = y })


-- Env
instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
instance HasConfigPath Env where
  configPathL = lens envConfigPath (\x y -> x { envConfigPath = y })
instance HasJobs Env where
  jobsL = lens envJobs (\x y -> x { envJobs = y })
instance HasGlobalCache Env where
  globalCacheL = lens envGlobalCache (\x y -> x { envGlobalCache = y })
instance HasCacheConfig Env where
  cacheConfigL = lens envCacheConfig (\x y -> x { envCacheConfig = y })
instance HasEnv Env where
  envL = id


-- PackageSetEnv
instance HasLogFunc PackageSetEnv where
  logFuncL = envL . logFuncL
instance HasGlobalCache PackageSetEnv where
  globalCacheL = envL . globalCacheL
instance HasConfigPath PackageSetEnv where
  configPathL = envL . configPathL
instance HasJobs PackageSetEnv where
  jobsL = envL . jobsL
instance HasPackageSet PackageSetEnv where
  packageSetL = lens packageSetSet (\x y -> x { packageSetSet = y })
instance HasEnv PackageSetEnv where 
  envL = lens packageSetEnvGlobal (\x y -> x { packageSetEnvGlobal = y })
instance HasPackageSetEnv PackageSetEnv


-- VerifyEnv
instance HasLogFunc VerifyEnv where
  logFuncL = envL . logFuncL
instance HasGlobalCache VerifyEnv where
  globalCacheL = envL . globalCacheL
instance HasConfigPath VerifyEnv where
  configPathL = envL . configPathL
instance HasJobs VerifyEnv where
  jobsL = envL . jobsL
instance HasCacheConfig VerifyEnv where
  cacheConfigL = envL . cacheConfigL
instance HasPurs VerifyEnv where
  pursL = lens verifyEnvPurs (\x y -> x { verifyEnvPurs = y })
instance HasPackageSet VerifyEnv where
  packageSetL = lens verifyPackageSet (\x y -> x { verifyPackageSet = y })
instance HasEnv VerifyEnv where 
  envL = lens verifyEnvGlobal (\x y -> x { verifyEnvGlobal = y })
instance HasVerifyEnv VerifyEnv 


-- Install env
instance HasLogFunc InstallEnv where
  logFuncL = envL . logFuncL
instance HasGlobalCache InstallEnv where
  globalCacheL = envL . globalCacheL
instance HasConfigPath InstallEnv where
  configPathL = envL . configPathL
instance HasJobs InstallEnv where
  jobsL = envL . jobsL
instance HasCacheConfig InstallEnv where
  cacheConfigL = envL . cacheConfigL
instance HasPackageSet InstallEnv where
  packageSetL = configL . packageSetL
instance HasConfig InstallEnv where
  configL = lens installSpagoConfig (\x y -> x { installSpagoConfig = y })
instance HasEnv InstallEnv where 
  envL = lens installEnvGlobal (\x y -> x { installEnvGlobal = y })
instance HasInstallEnv InstallEnv where 
  installEnvL = id


-- PublishEnv
instance HasLogFunc PublishEnv where 
  logFuncL = envL . logFuncL
instance HasGlobalCache PublishEnv where
  globalCacheL = envL . globalCacheL
instance HasConfigPath PublishEnv where
  configPathL = envL . configPathL
instance HasJobs PublishEnv where
  jobsL = envL . jobsL
instance HasBower PublishEnv where 
  bowerL = lens publishEnvBower (\x y -> x { publishEnvBower = y })
instance HasGit PublishEnv where 
  gitL = lens publishEnvGit (\x y -> x { publishEnvGit = y })
instance HasCacheConfig PublishEnv where
  cacheConfigL = installEnvL . cacheConfigL
instance HasPackageSet PublishEnv where
  packageSetL = installEnvL . packageSetL
instance HasConfig PublishEnv where
  configL = installEnvL . configL
instance HasEnv PublishEnv where
  envL = installEnvL . envL
instance HasInstallEnv PublishEnv where
  installEnvL = lens publishEnvInstall (\x y -> x { publishEnvInstall = y })
instance HasPublishEnv PublishEnv 


-- BuildEnv
instance HasLogFunc BuildEnv where 
  logFuncL = envL . logFuncL
instance HasGlobalCache BuildEnv where
  globalCacheL = envL . globalCacheL
instance HasConfigPath BuildEnv where
  configPathL = envL . configPathL
instance HasJobs BuildEnv where
  jobsL = envL . jobsL
instance HasPurs BuildEnv where
  pursL = lens buildEnvPurs (\x y -> x { buildEnvPurs = y })
instance HasCacheConfig BuildEnv where
  cacheConfigL = installEnvL . cacheConfigL
instance HasPackageSet BuildEnv where
  packageSetL = installEnvL . packageSetL
instance HasConfig BuildEnv where
  configL = installEnvL . configL
instance HasEnv BuildEnv where
  envL = installEnvL . envL
instance HasInstallEnv BuildEnv where
  installEnvL = lens buildInstallEnv (\x y -> x { buildInstallEnv = y })
instance HasBuildEnv BuildEnv