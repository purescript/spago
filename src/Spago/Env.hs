module Spago.Env
  ( 
  -- | Global environment
    Env(..)
  , HasEnv(..)
  , HasGlobalCache(..)
  , HasConfigPath(..)
  , HasJobs(..)
  , HasPsa(..)

  -- | Build environment
  , BuildEnv(..)
  , HasBuildEnv(..)
  , HasPurs(..)

  -- | Flags
  , UsePsa(..)

  ) where

import RIO

import qualified GHC.IO


-- | Flag to disable the automatic use of `psa`
data UsePsa = UsePsa | NoPsa

-- | App configuration containing parameters and other common
--   things it's useful to compute only once at startup.
data Env = Env
  { envUsePsa      :: !UsePsa
  , envJobs        :: !Int
  , envConfigPath  :: !Text
  , envGlobalCache :: !GHC.IO.FilePath
  , envLogFunc     :: !LogFunc
  }

-- | Configuration that is needed to run a build
data BuildEnv = BuildEnv 
  { buildEnvGlobal :: !Env 
  -- ^ the global configuration
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

class HasPsa env where
  usePsaL :: Lens' env UsePsa

class HasPurs env where
  pursL :: Lens' env Text

class
  ( HasGlobalCache env
  , HasLogFunc env
  , HasConfigPath env
  , HasJobs env
  , HasPsa env
  ) => HasEnv env where
  envL :: Lens' env Env

class
  ( HasPurs env
  , HasEnv env
  ) => HasBuildEnv env where
  buildEnvL :: Lens' env BuildEnv


-- ### Instances

-- Log func
instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

instance HasLogFunc BuildEnv where 
  logFuncL = envL . logFuncL

-- Global cache
instance HasGlobalCache Env where
  globalCacheL = lens envGlobalCache (\x y -> x { envGlobalCache = y })

instance HasGlobalCache BuildEnv where
  globalCacheL = envL . globalCacheL

-- Config path
instance HasConfigPath Env where
  configPathL = lens envConfigPath (\x y -> x { envConfigPath = y })

instance HasConfigPath BuildEnv where
  configPathL = envL . configPathL

-- Jobs
instance HasJobs Env where
  jobsL = lens envJobs (\x y -> x { envJobs = y })

instance HasJobs BuildEnv where
  jobsL = envL . jobsL

-- HasPsa
instance HasPsa Env where
  usePsaL = lens envUsePsa (\x y -> x { envUsePsa = y })

instance HasPsa BuildEnv where
  usePsaL = envL . usePsaL

-- Purs
instance HasPurs BuildEnv where
  pursL = lens buildEnvPurs (\x y -> x { buildEnvPurs = y })

-- Env
instance HasEnv Env where
  envL = id

instance HasEnv BuildEnv where 
  envL = lens buildEnvGlobal (\x y -> x { buildEnvGlobal = y })


-- BuildEnv
instance HasBuildEnv BuildEnv where
  buildEnvL = id