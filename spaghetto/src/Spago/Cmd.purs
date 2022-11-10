module Spago.Cmd where

import Spago.Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (Pattern(..))
import Data.String as String
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn3)
import Partial.Unsafe (unsafeCrashWith)

data StdinConfig
  = StdinPipeParent
  | StdinNewPipe
  | StdinWrite String

type ExecOptions =
  { pipeStdin :: StdinConfig
  , pipeStdout :: Boolean
  , pipeStderr :: Boolean
  , cwd :: Maybe FilePath
  }

type ExecResult =
  { stdout :: String
  , stderr :: String
  , exitCode :: Int
  , failed :: Boolean
  , timedOut :: Boolean
  , isCanceled :: Boolean
  , killed :: Boolean
  }

type ExecError =
  { shortMessage :: String
  , exitCode :: Int
  , stdout :: String
  , stderr :: String
  , failed :: Boolean
  , timedOut :: Boolean
  , isCanceled :: Boolean
  , killed :: Boolean
  }

type ExecOptionsJS =
  { cwd :: Nullable String
  , shell :: Boolean
  , input :: Nullable String
  }

foreign import data ChildProcess :: Type

foreign import spawnImpl :: EffectFn3 String (Array String) ExecOptionsJS ChildProcess
foreign import spawnCommandImpl :: EffectFn2 String ExecOptionsJS ChildProcess
foreign import killImpl :: EffectFn1 ChildProcess Unit
foreign import pipeStdinImpl :: EffectFn1 ChildProcess Unit
foreign import pipeStdoutImpl :: EffectFn1 ChildProcess Unit
foreign import pipeStderrImpl :: EffectFn1 ChildProcess Unit
foreign import joinImpl :: forall a b r. EffectFn3 ChildProcess (a -> r) (b -> r) (Promise r)

defaultExecOptions :: ExecOptions
defaultExecOptions =
  { pipeStdin: StdinNewPipe
  , pipeStdout: true
  , pipeStderr: true
  , cwd: Nothing
  }

spawn :: forall m. MonadEffect m => String -> Array String -> ExecOptions -> m ChildProcess
spawn cmd args opts = liftEffect do
  subprocess <- runEffectFn3 spawnImpl cmd args
    { cwd: Nullable.toNullable opts.cwd
    , input: case opts.pipeStdin of
        StdinWrite s -> Nullable.notNull s
        _ -> Nullable.null
    , shell: false
    }

  case opts.pipeStdin of
    StdinPipeParent -> runEffectFn1 pipeStdinImpl subprocess
    _ -> pure unit
  when (opts.pipeStderr) do
    runEffectFn1 pipeStderrImpl subprocess
  when (opts.pipeStdout) do
    runEffectFn1 pipeStdoutImpl subprocess

  pure subprocess

joinProcess :: forall m. MonadAff m => ChildProcess -> m (Either ExecError ExecResult)
joinProcess cp = liftAff do
  let res = runEffectFn3 joinImpl cp Left Right
  Promise.toAffE res

exec :: forall m. MonadAff m => String -> Array String -> ExecOptions -> m (Either ExecError ExecResult)
exec cmd args opts = do
  res <- spawn cmd args opts
  joinProcess res

kill :: ChildProcess -> Aff ExecError
kill cp = do
  liftEffect (runEffectFn1 killImpl cp)
  joinProcess cp >>= case _ of
    Left e -> pure e
    Right res -> unsafeCrashWith ("Tried to kill the process, failed. Result: " <> show res)

-- | Try to find one of the flags in a list of Purs args
-- TODO: this code needs some comments and a test
findFlag :: { flags :: Array String, args :: Array String } -> Maybe String
findFlag { flags, args } = case Array.uncons args of
  Just { head: x, tail: xs } ->
    if isFlag x then case Array.uncons xs of
      Just { head: y } -> Just y
      _ -> Nothing
    else if hasFlag x then case words x of
      [ word ] -> case splitOnEqual word of
        [ _, value ] -> Just value
        _ -> Nothing
      [ _, value, _ ] -> Just value
      _ -> Nothing
    else findFlag { flags, args: xs }
  _ -> Nothing
  where
  words = String.split (Pattern " ")
  splitOnEqual = String.split (Pattern "=")

  isFlag :: String -> Boolean
  isFlag word = isJust $ Array.find (_ == word) flags

  hasFlag :: String -> Boolean
  hasFlag a = isJust $ Array.find (_ == firstWord) flags
    where
    firstWord = fromMaybe "" $ case Array.uncons (words a) of
      Just { head: h1, tail } -> case tail of
        [] -> case Array.uncons (splitOnEqual h1) of
          Just { head: h2 } -> Just h2
          _ -> Nothing
        _ -> Just h1
      _ -> Nothing
