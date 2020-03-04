module Spago.Env
  ( Env(..)
  , HasEnv(..)
  , HasGlobalCache(..)
  , HasConfigPath(..)
  , HasJobs(..)
  , HasPsa(..)
  , UsePsa(..)
  ) where

import RIO

import qualified GHC.IO


-- | Flag to disable the automatic use of `psa`
data UsePsa = UsePsa | NoPsa

-- | App configuration containing parameters and other common
--   things it's useful to compute only once at startup.
data Env = Env
  { envUsePsa      :: UsePsa
  , envJobs        :: Int
  , envConfigPath  :: Text
  , envGlobalCache :: GHC.IO.FilePath
  , envLogFunc     :: !LogFunc
  }


class HasGlobalCache env where 
  globalCacheL :: Lens' env GHC.IO.FilePath 

class HasConfigPath env where 
  configPathL :: Lens' env Text

class HasJobs env where 
  jobsL :: Lens' env Int

class HasPsa env where
  usePsaL :: Lens' env UsePsa

class 
  ( HasGlobalCache env
  , HasLogFunc env
  , HasConfigPath env
  , HasJobs env
  , HasPsa env
  ) => HasEnv env where
  envL :: Lens' env Env

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

instance HasGlobalCache Env where
  globalCacheL = lens envGlobalCache (\x y -> x { envGlobalCache = y })

instance HasConfigPath Env where
  configPathL = lens envConfigPath (\x y -> x { envConfigPath = y })

instance HasJobs Env where
  jobsL = lens envJobs (\x y -> x { envJobs = y })

instance HasPsa Env where
  usePsaL = lens envUsePsa (\x y -> x { envUsePsa = y })

instance HasEnv Env where
  envL = id