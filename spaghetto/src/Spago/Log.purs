module Spago.Log where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console

-- TODO: https://www.npmjs.com/package/chalk

log :: forall m. MonadEffect m => String -> m Unit
log = Console.log

logShow :: forall m a. MonadEffect m => Show a => a -> m Unit
logShow = Console.logShow
