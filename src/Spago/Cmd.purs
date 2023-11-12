module Spago.Cmd where

import Spago.Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Node.Library.Execa as Execa
import Node.Platform as Platform
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

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
      askVersion cmd1 >>= case _ of
        Right r -> pure { cmd: cmd1, output: r.stdout }
        Left err' -> do
          let cmd2 = mkCmd command Nothing
          logDebug [ "Failed to find ", command, ".cmd. Trying with just purs...", show err' ]
          askVersion cmd2 >>= case _ of
            Right r -> pure { cmd: cmd2, output: r.stdout }
            Left err -> complain err
    _ -> do
      -- On other platforms, we just call `purs`
      let cmd1 = mkCmd command Nothing
      askVersion cmd1 >>= case _ of
        Right r -> pure { cmd: cmd1, output: r.stdout }
        Left err -> complain err
  where
  askVersion cmd = exec cmd [ "--version" ] defaultExecOptions { pipeStdout = false, pipeStderr = false }

  mkCmd cmd maybeExtension = cmd <> maybe "" (append ".") maybeExtension

  complain err = do
    logDebug $ show err
    die [ "Failed to find " <> command <> ". Have you installed it, and is it in your PATH?" ]
