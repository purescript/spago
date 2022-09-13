module Spago.Log
  ( LogEnv
  , LogOptions
  , LogVerbosity(..)
  , class Loggable
  , die
  , logDebug
  , logError
  , logInfo
  , logSuccess
  , logWarn
  , supportsColor
  , toDoc
  ) where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Reader as Reader
import Dodo (Doc)
import Dodo as Log
import Dodo.Ansi (GraphicsParam)
import Dodo.Ansi as Ansi
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class as Effect
import Effect.Class.Console as Console
import Node.Process as Process

type LogEnv a = { logOptions :: LogOptions | a }

type LogOptions = { color :: Boolean, verbosity :: LogVerbosity }

data LogVerbosity
  = LogQuiet
  | LogNormal
  | LogVerbose

-- | LogVeryVerbose -- TODO:we'll need to add timestamps, and locations, see https://stackoverflow.com/questions/45395369/

data LogLevel
  = LogDebug
  | LogInfo
  | LogWarning
  | LogError

type Log = { content :: Doc GraphicsParam, level :: LogLevel }

class Loggable a where
  toDoc :: a -> Doc GraphicsParam

instance Loggable (Doc GraphicsParam) where
  toDoc = identity

instance Loggable String where
  toDoc = Log.text

instance Loggable a => Loggable (Array a) where
  toDoc = Log.lines <<< map toDoc

log :: forall a m. MonadEffect m => MonadAsk (LogEnv a) m => Log -> m Unit
log { content, level } = do
  { logOptions } <- Reader.ask
  let printFn = if logOptions.color then Log.print Ansi.ansiGraphics else Log.print Log.plainText
  case logOptions.verbosity, level of
    LogQuiet, _ -> pure unit
    LogNormal, LogDebug -> pure unit
    _, _ -> Console.error $ printFn (Log.twoSpaces { pageWidth = 200 }) content

logInfo :: forall a b m. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m Unit
logInfo l = log { level: LogInfo, content: toDoc l }

logSuccess :: forall a b m. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m Unit
logSuccess l = log
  { level: LogInfo
  , content: Ansi.foreground Ansi.Green
      (Log.break <> Ansi.bold (toDoc " ✓" <> Log.space <> toDoc l) <> Log.break)
  }

logDebug :: forall a b m. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m Unit
logDebug l = log { level: LogDebug, content: Ansi.foreground Ansi.Blue (toDoc l) }

logWarn :: forall a b m. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m Unit
logWarn l = log { level: LogWarning, content: Ansi.foreground Ansi.Yellow (toDoc l) }

logError :: forall a b m. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m Unit
logError l = log { level: LogError, content: Ansi.foreground Ansi.Red (toDoc l) }

die :: forall a b m u. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m u
die msg = do
  logError msg
  Effect.liftEffect $ Process.exit 1

foreign import supportsColor :: Effect Boolean
