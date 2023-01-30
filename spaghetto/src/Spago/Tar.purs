module Spago.Tar where

import Spago.Prelude

import Data.Function.Uncurried (Fn4, runFn4)

foreign import extractImpl :: forall r. Fn4 (TarErr -> r) (Unit -> r) String String (Effect r)

type TarErr =
  { recoverable :: Boolean
  , file :: FilePath
  , cwd :: FilePath
  , code :: String
  , tarCode :: String
  }

-- | Extracts the tarball at the given filename into cwd.
-- |
-- | Note: `filename` should be an absolute path. The extracted result will be
-- | a directory within `cwd`.
extract :: ExtractArgs -> Effect (Either TarErr Unit)
extract { cwd, filename } = runFn4 extractImpl Left Right cwd filename

type ExtractArgs = { cwd :: String, filename :: String }
