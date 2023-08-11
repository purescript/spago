-- | This module provides support for the
-- | PureScript interactive mode, PSCI.

module PSCI.Support where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)

-- | The `Eval` class captures those types which can be
-- | evaluated in the REPL.
-- |
-- | There are instances provided for the `Effect` type
-- | constructor and any `Show`able types.
class Eval a where
  eval :: a -> Effect Unit

instance evalEffectUnit :: Eval (Effect Unit) where
  eval = identity
else
instance evalEffect :: Eval a => Eval (Effect a) where
  eval x = x >>= eval
else
instance evalShow :: Show a => Eval a where
  eval = logShow

