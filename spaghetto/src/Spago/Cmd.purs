module Spago.Cmd where

import Spago.Prelude

import Data.Array as Array
import Data.Nullable (Nullable)
import Data.String (Pattern(..))
import Data.String as String
import Node.Library.Execa as Execa
import Partial.Unsafe (unsafeCrashWith)
import Data.Time.Duration (Milliseconds(..))

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

type ExecResult = Execa.ExecaSuccess

type ExecError = Execa.ExecaError

type ExecOptionsJS =
  { cwd :: Nullable String
  , shell :: Boolean
  , input :: Nullable String
  }

foreign import data ChildProcess :: Type

defaultExecOptions :: ExecOptions
defaultExecOptions =
  { pipeStdin: StdinNewPipe
  , pipeStdout: true
  , pipeStderr: true
  , cwd: Nothing
  }

spawn :: forall m. MonadAff m => String -> Array String -> ExecOptions -> m Execa.ExecaProcess
spawn cmd args opts = liftAff do
  subprocess <- Execa.execa cmd args (_ { cwd = opts.cwd })

  case opts.pipeStdin of
    StdinPipeParent -> subprocess.stdin.shareParentProcessStdin
    StdinWrite s -> subprocess.stdin.writeUtf8End s
    _ -> pure unit
  when (opts.pipeStderr) do
    subprocess.stderr.pipeToParentStderr
  when (opts.pipeStdout) do
    subprocess.stdout.pipeToParentStdout

  pure subprocess

joinProcess :: forall m. MonadAff m => Execa.ExecaProcess -> m (Either ExecError ExecResult)
joinProcess cp = liftAff $ cp.result

exec :: forall m. MonadAff m => String -> Array String -> ExecOptions -> m (Either ExecError ExecResult)
exec cmd args opts = liftAff do
  subprocess <- spawn cmd args opts
  subprocess.result

kill :: Execa.ExecaProcess -> Aff ExecError
kill cp = liftAff do
  void $ cp.killForced $ Milliseconds 2_000.0
  cp.result >>= case _ of
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
