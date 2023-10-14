module Spago.Log
  ( Docc
  , LogEnv
  , LogOptions
  , LogVerbosity(..)
  , OutputFormat(..)
  , class Loggable
  , die
  , die'
  , justOrDieWith
  , justOrDieWith'
  , rightOrDieWith
  , rightOrDieWith'
  , indent2
  , logDebug
  , logError
  , logFailure
  , logInfo
  , logSuccess
  , logWarn
  , module DodoExport
  , output
  , toDoc
  ) where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Reader as Reader
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray) as NEA
import Data.Codec.Argonaut (JsonCodec)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (traverse)
import Dodo (Doc, print, twoSpaces)
import Dodo (indent, break) as DodoExport
import Dodo as Dodo
import Dodo as Log
import Dodo.Ansi (GraphicsParam)
import Dodo.Ansi (bold) as DodoExport
import Dodo.Ansi as Ansi
import Dodo.Box (DocBox)
import Dodo.Box as Box
import Effect.Class (class MonadEffect)
import Effect.Class as Effect
import Effect.Class.Console as Console
import Node.Process as Process
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Spago.Json as Json
import Spago.Yaml as Yaml

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

type Docc = Doc GraphicsParam

type Log = { content :: Docc, level :: LogLevel }

class Loggable a where
  toDoc :: a -> Docc

instance Loggable Docc where
  toDoc = identity

instance Loggable String where
  toDoc = Log.text

instance Loggable PackageName where
  toDoc = PackageName.print >>> toDoc

instance Loggable a => Loggable (Array a) where
  toDoc = Log.lines <<< map toDoc

instance Loggable a => Loggable (NonEmptyArray a) where
  toDoc = Log.lines <<< NEA.toArray <<< map toDoc

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
      (Log.break <> Ansi.bold (toDoc "✅" <> Log.space <> toDoc l) <> Log.break)
  }

logFailure :: forall a b m. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m Unit
logFailure l = log
  { level: LogInfo
  , content: Ansi.foreground Ansi.Red
      (Log.break <> Ansi.bold (toDoc "❌" <> Log.space <> toDoc l) <> Log.break)
  }

logDebug :: forall a b m. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m Unit
logDebug l = log { level: LogDebug, content: Ansi.foreground Ansi.Blue (toDoc l) }

logWarn :: forall a b m. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m Unit
logWarn l = log
  { level: LogWarning
  , content: Ansi.foreground Ansi.Yellow (Ansi.bold (toDoc "⚠️" <> Log.space <> toDoc l))
  }

logError :: forall a b m. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m Unit
logError l = log { level: LogError, content: Ansi.foreground Ansi.Red (toDoc l) }

die :: forall a b m u. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => a -> m u
die msg = do
  logFailure msg
  Effect.liftEffect $ Process.exit 1

-- | Same as `die`, but with multiple failures
die' :: forall a b m u. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => Array a -> m u
die' msgs = do
  _ <- traverse logFailure msgs
  Effect.liftEffect $ Process.exit 1

justOrDieWith :: forall a b m x. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => Maybe x -> a -> m x
justOrDieWith value msg = case value of
  Just a ->
    pure a
  Nothing ->
    die msg

justOrDieWith' :: forall a b m x. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => Maybe x -> Array a -> m x
justOrDieWith' value msg = case value of
  Just a ->
    pure a
  Nothing ->
    die' msg

rightOrDieWith :: forall a b m err x. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => Either err x -> (err -> a) -> m x
rightOrDieWith value toMsg = case value of
  Right a ->
    pure a
  Left err ->
    die $ toMsg err

rightOrDieWith' :: forall a b m err x. MonadEffect m => MonadAsk (LogEnv b) m => Loggable a => Either err x -> (err -> Array a) -> m x
rightOrDieWith' value toMsg = case value of
  Right a ->
    pure a
  Left err ->
    die' $ toMsg err

data OutputFormat a
  = OutputJson (JsonCodec a) a
  | OutputYaml (JsonCodec a) a
  | OutputTable { titles :: Array String, rows :: Array (Array String) }
  | OutputLines (Array String)

output :: forall a m. MonadEffect m => OutputFormat a -> m Unit
output format = Console.log case format of
  OutputJson codec json -> Json.printJson codec json
  OutputYaml codec yaml -> Yaml.printYaml codec yaml
  OutputLines lines -> String.joinWith "\n" lines
  -- https://github.com/natefaubion/purescript-dodo-printer/blob/master/test/snapshots/DodoBox.purs
  OutputTable { titles, rows } ->
    Log.print Log.plainText (Log.twoSpaces)
      $ Box.toDoc
      $ logTable { headers: textBox <$> titles, rows: (map textBox) <$> rows }

textBox :: forall a. String -> DocBox a
textBox = print Box.docBox twoSpaces <<< Dodo.text

logTable
  :: forall a
   . { headers :: Array (DocBox a), rows :: Array (Array (DocBox a)) }
  -> DocBox a
logTable { headers, rows } =
  Box.vertical
    [ rowSep
    , Box.vertical $ columns headers : rowSep : (columns <$> rows)
    , rowSep
    ]
  where
  joint = Box.fill (Log.text "+") { width: 1, height: 1 }

  rowSep =
    Box.horizontal
      [ joint
      , Box.horizontal
          $ Array.intersperse joint
          $ map (\width -> Box.fill (Dodo.text "-") { width: width + 2, height: 1 }) widths
      , joint
      ]

  columns cols = do
    let
      height = Array.foldr (max <<< _.height <<< Box.sizeOf) 0 cols
      sep = Box.fill (Dodo.text "|") { width: 1, height }
      colBoxes = Array.mapWithIndex
        ( \ix col ->
            Box.horizontal
              [ Box.hpadding 1
              , Box.resize { width: fromMaybe 0 (Array.index widths ix), height } col
              , Box.hpadding 1
              ]
        )
        cols

    Box.horizontal
      [ sep
      , Box.horizontal $ Array.intersperse sep colBoxes
      , sep
      ]

  widths = Array.mapWithIndex
    ( \ix hd ->
        Array.foldr
          ( flip Array.index ix
              >>> map (_.width <<< Box.sizeOf)
              >>> fromMaybe 0
              >>> max
          )
          (Box.sizeOf hd).width
          rows
    )
    headers

indent2 :: forall a. Doc a -> Doc a
indent2 = Log.indent <<< Log.indent
