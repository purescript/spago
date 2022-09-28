module Spago.Env where

import Spago.Prelude

type Env =
  { logFn :: String -> Effect Unit
  }

-- TODO: cache, maybe config path?
