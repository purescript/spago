module Spago.Tar where

import Spago.Prelude

import Data.Function.Uncurried (Fn2, runFn2)

foreign import extractImpl :: Fn2 String String (Effect Unit)

-- | Extracts the tarball at the given filename into cwd.
-- |
-- | Note: `filename` should be an absolute path. The extracted result will be
-- | a directory within `cwd`.
extract :: ExtractArgs -> Effect Unit
extract { cwd, filename } = runFn2 extractImpl cwd filename

type ExtractArgs = { cwd :: String, filename :: String }
