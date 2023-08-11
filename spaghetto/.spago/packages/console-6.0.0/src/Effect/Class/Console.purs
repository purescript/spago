module Effect.Class.Console where

import Data.Function ((<<<))
import Data.Show (class Show)
import Data.Unit (Unit)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as EffConsole

log :: forall m. MonadEffect m => String -> m Unit
log = liftEffect <<< EffConsole.log

logShow :: forall m a. MonadEffect m => Show a => a -> m Unit
logShow = liftEffect <<< EffConsole.logShow

warn :: forall m. MonadEffect m => String -> m Unit
warn = liftEffect <<< EffConsole.warn

warnShow :: forall m a. MonadEffect m => Show a => a -> m Unit
warnShow = liftEffect <<< EffConsole.warnShow

error :: forall m. MonadEffect m => String -> m Unit
error = liftEffect <<< EffConsole.error

errorShow :: forall m a. MonadEffect m => Show a => a -> m Unit
errorShow = liftEffect <<< EffConsole.errorShow

info :: forall m. MonadEffect m => String -> m Unit
info = liftEffect <<< EffConsole.info

infoShow :: forall m a. MonadEffect m => Show a => a -> m Unit
infoShow = liftEffect <<< EffConsole.infoShow

debug :: forall m. MonadEffect m => String -> m Unit
debug = liftEffect <<< EffConsole.debug

debugShow :: forall m a. MonadEffect m => Show a => a -> m Unit
debugShow = liftEffect <<< EffConsole.debugShow

time :: forall m. MonadEffect m => String -> m Unit
time = liftEffect <<< EffConsole.time

timeLog :: forall m. MonadEffect m => String -> m Unit
timeLog = liftEffect <<< EffConsole.timeLog

timeEnd :: forall m. MonadEffect m => String -> m Unit
timeEnd = liftEffect <<< EffConsole.timeEnd

clear :: forall m. MonadEffect m => m Unit
clear = liftEffect EffConsole.clear
