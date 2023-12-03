module Spago.Cmd where

import Spago.Prelude

import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Posix (Pid)
import Data.String (Pattern(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Node.ChildProcess.Types (Exit(..), KillSignal, inherit, pipe)
import Node.Library.Execa as Execa
import Node.Platform as Platform
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

data StdinConfig
  = StdinPipeParent
  | StdinNewPipe
  | StdinWrite String

type ExecResult =
  { canceled :: Boolean
  , escapedCommand :: String
  , exit :: Exit
  , exitCode :: Maybe Int
  , killed :: Boolean
  , message :: String
  , originalMessage :: Maybe String
  , pid :: Maybe Pid
  , shortMessage :: String
  , signal :: Maybe KillSignal
  , signalDescription :: Maybe String
  , stderr :: String
  , stderrError :: Maybe Error
  , stdinError :: Maybe Error
  , stdout :: String
  , stdoutError :: Maybe Error
  , timedOut :: Boolean
  }

printExecResult :: ExecResult -> String
printExecResult r = Array.intercalate "\n"
  [ "escapedCommand: " <> show r.escapedCommand
  , "canceled: " <> show r.canceled
  , "exit: " <> show r.exit
  , "exitCode: " <> show r.exitCode
  , "signal: " <> show r.signal
  , "signalDescription: " <> show r.signalDescription
  , "pid: " <> show r.pid
  , "killed: " <> show r.killed
  , "timedOut: " <> show r.timedOut
  , "shortMessage: " <> show r.shortMessage
  , "message: " <> show r.message
  , "originalMessage: " <> show r.originalMessage
  , "stdinError: " <> show r.stdinError
  , "stdoutError: " <> show r.stdoutError
  , "stderrError: " <> show r.stderrError
  , "stderr:"
  , r.stderr
  , ""
  , "stdout:"
  , r.stdout
  , ""
  ]

-- | True if the process exited normally with exit code 0
isSuccess :: ExecResult -> Boolean
isSuccess r = case r.exit of
  Normally 0 -> true
  _ -> false

-- | True if the process exited with a non-zero exit code or was terminated by a signal
isFailure :: ExecResult -> Boolean
isFailure = not <<< isSuccess

type ExecOptions =
  { pipeStdin :: StdinConfig
  , pipeStdout :: Boolean
  , pipeStderr :: Boolean
  , cwd :: Maybe FilePath
  }

defaultExecOptions :: ExecOptions
defaultExecOptions =
  { pipeStdin: StdinNewPipe
  , pipeStdout: true
  , pipeStderr: true
  , cwd: Nothing
  }

spawn :: forall m. MonadAff m => String -> Array String -> ExecOptions -> m Execa.ExecaProcess
spawn cmd args opts = liftAff do
  let
    stdinOpt = case opts.pipeStdin of
      StdinPipeParent -> Just inherit
      StdinWrite _ -> Just pipe
      StdinNewPipe -> Just pipe
  subprocess <- Execa.execa cmd args (_ { cwd = opts.cwd, stdin = stdinOpt, stdout = Just pipe, stderr = Just pipe })

  case opts.pipeStdin of
    StdinWrite s | Just { writeUtf8End } <- subprocess.stdin -> writeUtf8End s
    _ -> pure unit

  when (opts.pipeStderr) do
    traverse_ _.pipeToParentStderr subprocess.stderr
  when (opts.pipeStdout) do
    traverse_ _.pipeToParentStdout subprocess.stdout

  pure subprocess

joinProcess :: forall m. MonadAff m => Execa.ExecaProcess -> m ExecResult
joinProcess cp = liftAff $ cp.getResult

exec :: forall m. MonadAff m => String -> Array String -> ExecOptions -> m ExecResult
exec cmd args opts = liftAff do
  subprocess <- spawn cmd args opts
  subprocess.getResult

kill :: Execa.ExecaProcess -> Aff ExecResult
kill cp = liftAff do
  void $ cp.killForced $ Milliseconds 2_000.0
  cp.getResult >>= \r -> case isSuccess r of
    false -> pure r
    true -> unsafeCrashWith ("Tried to kill the process, failed. Result:\n" <> printExecResult r)

-- | Try to find one of the flags in a list of Purs args
-- | For example, trying to find the `output` arg
-- | we would run
-- | `findFlags { flags: [ "-o", "--output" ], args }`
-- | where `args` is one of the 5 variants below:
-- | - args are two separate array elements:
-- |    - `[ "-o", "dir"]`
-- |    - `[ "--output", "dir"]`
-- | - args are stored in the same array element
-- |    - `[ "-o dir"]`
-- |    - `[ "--output dir"]`
-- |    - `[ "--output=dir"]`
findFlag :: { flags :: Array String, args :: Array String } -> Maybe String
findFlag { flags, args } = if len == 0 then Nothing else go 0
  where
  len = Array.length args
  lastIdx = len - 1
  go idx = do
    let arg = unsafePartial $ Array.unsafeIndex args idx
    case Array.findMap (\flag -> stripFlag flag arg) flags of
      Just (Tuple isOneCharFlag restOfArg)
        | restOfArg == "" -> do
            Array.index args $ idx + 1
        | otherwise ->
            dropExtra isOneCharFlag restOfArg
      Nothing
        | idx < lastIdx ->
            go $ idx + 1
        | otherwise ->
            Nothing

  stripFlag flag arg =
    Tuple (isSingleCharFlag flag) <$> String.stripPrefix (Pattern flag) arg

  dropExtra isOneCharFlag restOfArg =
    if isOneCharFlag then dropSpace restOfArg
    else dropSpace restOfArg <|> dropEquals restOfArg
    where
    dropSpace = String.stripPrefix (Pattern " ")
    dropEquals = String.stripPrefix (Pattern "=")

  isSingleCharFlag = eq (Just 1) <<< map String.length <<< String.stripPrefix (Pattern "-")

getExecutable :: forall a. String -> Spago (LogEnv a) { cmd :: String, output :: String }
getExecutable command =
  case Process.platform of
    Just Platform.Win32 -> do
      -- On Windows, we often need to call the `.cmd` version
      let cmd1 = mkCmd command (Just "cmd")
      askVersion cmd1 >>= \r -> case isSuccess r of
        true -> pure { cmd: cmd1, output: r.stdout }
        false -> do
          let cmd2 = mkCmd command Nothing
          logDebug [ "Failed to find purs.cmd. Trying with just purs...", show r.message ]
          askVersion cmd2 >>= \r' -> case isSuccess r' of
            true -> pure { cmd: cmd2, output: r'.stdout }
            false -> complain r
    _ -> do
      -- On other platforms, we just call `purs`
      let cmd1 = mkCmd command Nothing
      askVersion cmd1 >>= \r -> case isSuccess r of
        true -> pure { cmd: cmd1, output: r.stdout }
        false -> complain r
  where
  askVersion cmd = exec cmd [ "--version" ] defaultExecOptions { pipeStdout = false, pipeStderr = false }

  mkCmd cmd maybeExtension = cmd <> maybe "" (append ".") maybeExtension

  complain err = do
    logDebug $ printExecResult err
    die [ "Failed to find " <> command <> ". Have you installed it, and is it in your PATH?" ]
