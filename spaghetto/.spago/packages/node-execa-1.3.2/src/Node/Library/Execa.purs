-- | Provides a higher-level replacement to Node.js `child_process` module.
-- | Uses sane defaults with clearer error messages.
-- | - `spawn`/`spawnSync` -> `execa`/`execaSync`
-- | - `exec`/`execSync` -> `execaCommand`/`execaCommandSync`
-- | - `fork` - has no equivalent
module Node.Library.Execa
  ( ExecaError
  , ExecaOptions
  , ExecaProcess
  , ExecaSuccess
  , execa
  , ExecaSyncOptions
  , ExecaSyncResult
  , execaSync
  , execaCommand
  , execaCommandSync
  , Handle
  , ChildProcessError
  ) where

import Prelude

import Control.Alternative ((<|>), guard)
import Control.Monad.Except (runExcept)
import Control.Parallel (parOneOf)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (for_, sequence_)
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Int (floor, toNumber)
import Data.Lens (Prism', is, preview, prism)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Posix (Pid)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.Regex (Regex, test)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds(..), effectCanceler, finally, forkAff, joinFiber, makeAff, never, nonCanceler, suspendAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn5, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn5)
import Foreign (Foreign, readInt, readString, renderForeignError, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (unsafeThaw)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Immutable as ImmutableBuffer
import Node.Buffer.Internal as Buffer
import Node.Encoding (Encoding(..))
import Node.Library.Execa.CrossSpawn (CrossSpawnConfig)
import Node.Library.Execa.CrossSpawn as CrossSpawn
import Node.Library.Execa.GetStream (getStreamBuffer)
import Node.Library.Execa.NpmRunPath (defaultNpmRunPathOptions, npmRunPathEnv)
import Node.Library.Execa.ParseCommand (parseCommand)
import Node.Library.Execa.SignalExit as SignalExit
import Node.Library.Execa.StripFinalNewline (stripFinalNewlineBuf)
import Node.Library.HumanSignals (signals)
import Node.Process as Process
import Node.Stream (Readable, Writable, destroy)
import Node.Stream as Stream
import Node.Stream as Streams
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Updates the given `env`:
-- | - if `extendEnv` is enabled, unions it with `Process.env`
-- | - if `preferLocal` is enabled, prepends the current directory
-- | and its parents' `node_modules/.bin` to `PATH`
getEnv
  :: { env :: Object String
     , extendEnv :: Boolean
     , preferLocal :: Maybe { localDir :: Maybe String, execPath :: Maybe String }
     }
  -> Effect (Object String)
getEnv r = do
  processEnv <- Process.getEnv
  let
    env = if r.extendEnv then Object.union r.env processEnv else r.env
  case r.preferLocal of
    Nothing ->
      pure env
    Just options ->
      npmRunPathEnv env $ defaultNpmRunPathOptions { cwd = options.localDir, execPath = options.execPath }

-- | - `cleanup` (default: `true`): Kill the spawned process when the parent process exits unless either:
-- |  - the spawned process is `detached`
-- |  - the parent process is terminated abruptly, for example, with SIGKILL as opposed to SIGTERM or a normal exit
-- | `- preferLocal` (default: `Nothing`): When `Just`, includes and prefers locally-installed `node_modules/.bin` binaries
-- |   when looking for a binary to execute. In short, if you `npm install foo``, you can run `execa "foo"`.
-- |   `localDir` (if `Nothing`, `Process.cwd` is used) - Preferred path to find locally installed binaries in
-- |   `execPath` (if `Nothing`, `Process.execPath` is used) - Path to the Node.js executable to use in child processes. 
-- |      This can be either an absolute path or a path relative to the `localDir` option.
-- | - `stripFinalNewline` - (default: `true`). If enabled, trims the newline character of `stdout`/`stderr` (e.g. `/(?:/r/n)|\r|\n$/`
-- | - `extendEnv` (default: `true`) - Extends the child process' `env` with `Process.env`
-- | - `argv0` - see Node docs
-- | - `stdioExtra` - Append any other `stdio` values to the array.
-- |    The `stdio` array used is always `["pipe", "pipe", "pipe", "ipc"] <> fromMaybe [] options.stdioExtra`
-- | - `detached` - see Node docs
-- | - `uid` - see Node docs
-- | - `gid` - see Node docs
-- | - `shell` - see Node docs. The Boolean variant is not supported
-- | - `timeout` - the amount of time to wait before killing the child process with the given kill signal
-- | - `maxBuffer` - the amount of buffer space available to `stdout`/`stderr`.
-- |    If more data is written to their buffers, child process will error with a max buffer size exceeded error.
-- | - `windowsVerbatimArguments` - see Node docs
-- | - `windowsHide` - see Node docs
-- | - `windowsEnableCmdEcho` (default: `true`) - Enables the `\q` flag when using the `cmd` shell. See https://github.com/nodejs/node/issues/27120
-- |    This goes against the Windows' defaults but makes the `stdout`/`stderr` behavior more consistent across different operating systems.
type ExecaOptions =
  -- execa options
  { cleanup :: Maybe Boolean
  , preferLocal :: Maybe { localDir :: Maybe String, execPath :: Maybe String }
  , stripFinalNewline :: Maybe Boolean
  , extendEnv :: Maybe Boolean
  -- child process spawn options:
  , cwd :: Maybe String
  , env :: Maybe (Object String)
  , encoding :: Maybe Encoding
  , argv0 :: Maybe String
  , stdioExtra :: Maybe (Array Foreign)
  , detached :: Maybe Boolean
  , uid :: Maybe Int
  , gid :: Maybe Int
  , shell :: Maybe String
  , timeout :: Maybe { milliseconds :: Number, killSignal :: Either Int String }
  , maxBuffer :: Maybe Number
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  -- cross spawn options
  , windowsEnableCmdEcho :: Maybe Boolean
  }

defaultExecaOptions :: ExecaOptions
defaultExecaOptions =
  { cleanup: Nothing
  , preferLocal: Nothing
  , stdioExtra: Nothing
  , stripFinalNewline: Nothing
  , extendEnv: Nothing
  , encoding: Nothing
  , cwd: Nothing
  , env: Nothing
  , argv0: Nothing
  , detached: Nothing
  , uid: Nothing
  , gid: Nothing
  , shell: Nothing
  , timeout: Nothing
  , maxBuffer: Nothing
  , windowsVerbatimArguments: Nothing
  , windowsHide: Nothing
  , windowsEnableCmdEcho: Nothing
  }

defaultOptions
  :: { cleanup :: Boolean
     , extendEnv :: Boolean
     , maxBuffer :: Number
     , preferLocal ::
         Maybe
           { execPath :: Maybe String
           , localDir :: Maybe String
           }
     , encoding :: Encoding
     , stripFinalNewline :: Boolean
     , windowsEnableCmdEcho :: Boolean
     , windowsHide :: Boolean
     , windowsVerbatimArguments :: Boolean
     }
defaultOptions =
  { cleanup: true
  , preferLocal: Just { localDir: Nothing, execPath: Nothing }
  , stripFinalNewline: true
  , extendEnv: true
  , maxBuffer: toNumber $ 1_000 * 1_000 * 100 -- 100 MB
  , encoding: UTF8
  , windowsVerbatimArguments: false
  , windowsHide: true
  , windowsEnableCmdEcho: false
  }

handleArguments
  :: String
  -> Array String
  -> ExecaOptions
  -> Effect
       { file :: String
       , args :: Array String
       , options :: ExecaRunOptions
       , parsed :: CrossSpawnConfig
       }
handleArguments file args initOptions = do
  parsed <- CrossSpawn.parse file args
    { shell: initOptions.shell
    , env: initOptions.env
    , cwd: initOptions.cwd
    , windowsVerbatimArguments: Nothing
    , windowsEnableCmdEcho: fromMaybe defaultOptions.windowsEnableCmdEcho initOptions.windowsEnableCmdEcho
    }
  processCwd <- Process.cwd
  env <- getEnv
    { env: fromMaybe Object.empty initOptions.env
    , extendEnv: fromMaybe defaultOptions.extendEnv initOptions.extendEnv
    , preferLocal: initOptions.preferLocal
    }
  let
    -- validateTimeout
    { timeout, killSignal, timeoutWithKillSignal } = case initOptions.timeout of
      Just r | r.milliseconds > 0.0 ->
        { timeout: Just r.milliseconds, killSignal: Just r.killSignal, timeoutWithKillSignal: Just r }
      _ -> { timeout: Nothing, killSignal: Nothing, timeoutWithKillSignal: Nothing }

    options =
      { cleanup: fromMaybe defaultOptions.cleanup initOptions.cleanup
      , stdioExtra: fromMaybe [] initOptions.stdioExtra
      , maxBuffer: fromMaybe defaultOptions.maxBuffer initOptions.maxBuffer
      , stripFinalNewline: fromMaybe defaultOptions.stripFinalNewline initOptions.stripFinalNewline
      , cwd: fromMaybe processCwd initOptions.cwd
      , encoding: fromMaybe defaultOptions.encoding initOptions.encoding
      , env
      , argv0: initOptions.argv0
      , detached: fromMaybe false initOptions.detached
      , uid: initOptions.uid
      , gid: initOptions.gid
      , shell: initOptions.shell
      , timeout
      , killSignal
      , timeoutWithKillSignal
      , windowsHide: fromMaybe defaultOptions.windowsHide initOptions.windowsHide
      , windowsVerbatimArguments: fromMaybe defaultOptions.windowsVerbatimArguments parsed.options.windowsVerbatimArguments
      }
  pure { file: parsed.command, args: parsed.args, options, parsed }

-- | Re-exposes all the bindings for `ChildProcess`.
-- | In addition exposes, the following:
-- | `result` - gets the result of the process
-- | `cancel` - kill the child process, but indicate it was cancelled rather than killed in the error message
-- | `stdin.stream` - access the child process' `stdin`
-- | `stdin.writeUt8` - Write a string to the child process' `stdin`
-- | `stdin.writeUt8End` - Write a string to the child process' `stdin` and then `end` the stream
-- | `stdin.end` - End the child process' `stdin`
type ExecaProcess =
  { cancel :: Aff Unit
  , channel :: Aff (Maybe { ref :: Effect Unit, unref :: Effect Unit })
  , connected :: Aff Boolean
  , disconnect :: Aff Unit
  , exitCode :: Aff (Maybe Int)
  , kill :: Aff Boolean
  , killForced :: Milliseconds -> Aff Boolean
  , killForcedWithSignal :: Either Int String -> Milliseconds -> Aff Boolean
  , killWithSignal :: Either Int String -> Aff Boolean
  , killed :: Aff Boolean
  , onClose :: (Maybe Int -> Maybe String -> Effect Unit) -> Aff Unit
  , onDisconnect :: Effect Unit -> Aff Unit
  , onError :: (ChildProcessError -> Effect Unit) -> Aff Unit
  , onMessage :: (Foreign -> Maybe Handle -> Effect Unit) -> Aff Unit
  , onSpawn :: Effect Unit -> Aff Unit
  , pid :: Aff (Maybe Pid)
  , pidExists :: Aff Boolean
  , ref :: Aff Unit
  , result :: Aff (Either ExecaError ExecaSuccess)
  , send :: Foreign -> Handle -> ({ keepOpen :: Maybe Boolean } -> { keepOpen :: Maybe Boolean }) -> Effect Unit -> Aff Boolean
  , signalCode :: Aff (Maybe String)
  , spawnArgs :: Array String
  , spawnFile :: String
  , stdin ::
      { stream :: Writable ()
      , writeUtf8 :: String -> Aff Unit
      , writeUtf8End :: String -> Aff Unit
      , end :: Aff Unit
      , shareParentProcessStdin :: Aff Unit
      }
  , stdout ::
      { stream :: Readable ()
      , output :: Aff { text :: String, error :: Maybe Exception.Error }
      , pipeToParentStdout :: Aff Unit
      }
  , stderr ::
      { stream :: Readable ()
      , output :: Aff { text :: String, error :: Maybe Exception.Error }
      , pipeToParentStderr :: Aff Unit
      }
  , stdio :: Aff (Array Foreign)
  , unref :: Aff Unit
  }

type ExecaSuccess =
  { command :: String
  , escapedCommand :: String
  , exitCode :: Int
  , stderr :: String
  , stdout :: String
  }

-- | Replacement for `childProcess.spawn`. Since this is asynchronous,
-- | the returned value will not provide any results until one calls `spawned.result`:
-- | `execa ... >>= \spawned -> spawned.result`. 
-- |
-- | Override the default options using record update syntax.
-- | If defaults are good enough, just use `identity`.
-- | ```
-- | spawned <- execa "git checkout -b my-branch" (_
-- |    { cwd = Just $ Path.concat [ "some", "other", "directory"]
-- |    })
-- | spawned.result
-- |
-- | spawned2 <- execa "git checkout -b my-branch" identity
-- | spawned2.result
-- | ```
execa :: String -> Array String -> (ExecaOptions -> ExecaOptions) -> Aff ExecaProcess
execa file args buildOptions = do
  let options = buildOptions defaultExecaOptions
  parsed <- liftEffect $ handleArguments file args options
  let
    command = joinCommand file args
    escapedCommand = getEscapedCommand file args
  spawned <- liftEffect $ spawn parsed.file parsed.args
    { cwd: options.cwd
    , env: options.env
    , argv0: options.argv0
    , stdioExtra: options.stdioExtra
    , detached: options.detached
    , uid: options.uid
    , gid: options.gid
    , serialization: Nothing
    , shell: options.shell
    , windowsVerbatimArguments: options.windowsVerbatimArguments
    , windowsHide: options.windowsHide
    }
  spawnedFiber <- suspendAff $ makeAff \cb -> do
    onExit spawned \e s -> do
      case e, s of
        Just i, _ -> cb $ Right $ ExitCode i
        _, Just sig -> cb $ Right $ Killed $ fromKillSignal sig
        _, _ -> unsafeCrashWith "Impossible: either exit code or signal code must be non-null"

    onError spawned \error -> do
      cb $ Right $ SpawnError error

    Streams.onError (stdin spawned) \error -> do
      cb $ Right $ StdinError error
    pure nonCanceler
  timeoutFiber <- suspendAff do
    case parsed.options.timeoutWithKillSignal of
      Just { milliseconds, killSignal } -> do
        makeAff \cb -> do
          tid <- setTimeout ((unsafeCoerce :: Number -> Int) milliseconds) do
            void $ kill'' (toKillSignal killSignal) Nothing spawned
            void $ destroy (stdin spawned)
            void $ destroy (stdout spawned)
            void $ destroy (stderr spawned)
            cb $ Right $ TimedOut killSignal
          pure $ effectCanceler do
            clearTimeout tid
      _ ->
        never

  mainFiber <- suspendAff do
    parOneOf
      [ joinFiber spawnedFiber
      , joinFiber timeoutFiber
      ]

  processDoneFiber <- do
    if not parsed.options.cleanup || parsed.options.detached then pure mainFiber
    else suspendAff do
      removeHandlerRef <- liftEffect $ Ref.new Nothing
      finally
        (liftEffect $ Ref.read removeHandlerRef >>= sequence_)
        ( do
            liftEffect do
              removal <- SignalExit.onExit \_ _ -> do
                void $ kill'' (stringKillSignal "SIGTERM") Nothing spawned
              Ref.write (Just removal) removeHandlerRef
            joinFiber mainFiber
        )

  liftEffect $ runEffectFn2 monkeyPatchKill spawned spawnedKill
  isCanceledRef <- liftEffect $ Ref.new false
  -- PureScript implementaton note:
  -- We don't need to `handleInput` because
  -- we force end-users to write to `stdin` via
  -- its `Stream` interface.
  let
    cancel :: Aff Unit
    cancel = liftEffect do
      killSucceeded <- kill spawned
      when killSucceeded do
        Ref.write true isCanceledRef

    bufferToString = ImmutableBuffer.toString parsed.options.encoding

    mkStdIoFiber stream = forkAff do
      streamResult <- getStreamBuffer stream { maxBuffer: Just parsed.options.maxBuffer }
      text <- liftEffect do
        text <- bufferToString <$> handleOutput { stripFinalNewline: parsed.options.stripFinalNewline } streamResult.buffer
        when (isJust streamResult.inputError) do
          destroy stream
        pure text
      pure { text, error: streamResult.inputError }

  runFiber <- forkAff $ joinFiber processDoneFiber
  stdoutFiber <- mkStdIoFiber (stdout spawned)
  stderrFiber <- mkStdIoFiber (stderr spawned)

  let
    getSpawnResult = do
      { main: _, stdout: _, stderr: _ }
        <$> joinFiber runFiber
        <*> joinFiber stdoutFiber
        <*> joinFiber stderrFiber

  run <- forkAff do
    result <- getSpawnResult
    case result.main, result.stdout.error, result.stderr.error of
      ExitCode 0, Nothing, Nothing -> do
        pure $ Right
          { command
          , escapedCommand
          , exitCode: 0
          , stdout: result.stdout.text
          , stderr: result.stderr.text
          }
      someError, stdoutErr, stderrErr -> liftEffect do
        isCanceled <- Ref.read isCanceledRef
        killed' <- killed spawned
        pure $ Left $ mkError
          { error: preview _SpawnError someError
          , stdinErr: preview _StdinError someError
          , stdoutErr
          , stderrErr
          , exitCode: preview _ExitCode someError
          , signal: preview _Killed someError <|> preview _TimedOut someError
          , stdout: result.stdout.text
          , stderr: result.stderr.text
          , command
          , escapedCommand
          , parsed
          , timedOut: is _TimedOut someError
          , isCanceled
          , killed: killed'
          }

  pure
    { channel: liftEffect $ channel spawned
    , connected: liftEffect $ connected spawned
    , disconnect: liftEffect $ disconnect spawned
    , exitCode: liftEffect $ exitCode spawned
    , kill: liftEffect $ kill spawned
    , killWithSignal: \signal -> liftEffect do
        kill' (toKillSignal signal) spawned
    , killForced: \forceKillAfterTimeout -> liftEffect do
        kill'' (stringKillSignal "SIGTERM") (Just forceKillAfterTimeout) spawned
    , killForcedWithSignal: \signal forceKillAfterTimeout -> liftEffect do
        kill'' (toKillSignal signal) (Just forceKillAfterTimeout) spawned
    , pidExists: liftEffect $ pidExists spawned
    , killed: liftEffect $ killed spawned
    , pid: liftEffect $ pid spawned
    , unref: liftEffect $ unref spawned
    , ref: liftEffect $ ref spawned
    , send: \foreignData handle mkOptions cb -> liftEffect do
        send spawned foreignData handle mkOptions cb
    , signalCode: liftEffect $ signalCode spawned
    , spawnArgs: spawnArgs spawned
    , spawnFile: spawnFile spawned
    , onClose: \cb -> liftEffect $ onClose spawned cb
    , onDisconnect: \cb -> liftEffect $ onDisconnect spawned cb
    , onError: \cb -> liftEffect $ onError spawned cb
    , onMessage: \cb -> liftEffect $ onMessage spawned cb
    , onSpawn: \cb -> liftEffect $ onSpawn spawned cb
    , stdin:
        { stream: stdin spawned
        , writeUtf8: \string -> liftEffect do
            buf <- Buffer.fromString string UTF8
            void $ Stream.write (stdin spawned) buf mempty
        , writeUtf8End: \string -> liftEffect do
            buf <- Buffer.fromString string UTF8
            void $ Stream.write (stdin spawned) buf mempty
            void $ Stream.end (stdin spawned) mempty
        , end: liftEffect do
            void $ Stream.end (stdin spawned) mempty
        , shareParentProcessStdin: liftEffect do
            void $ Stream.pipe Process.stdin (stdin spawned)
        }
    , stdout:
        { stream: stdout spawned
        , output: joinFiber stdoutFiber
        , pipeToParentStdout: liftEffect do
            void $ Stream.pipe (stdout spawned) Process.stdout
        }
    , stderr:
        { stream: stderr spawned
        , output: joinFiber stderrFiber
        , pipeToParentStderr: liftEffect do
            void $ Stream.pipe (stderr spawned) Process.stderr
        }
    , stdio: liftEffect $ stdio spawned
    , cancel
    , result: joinFiber run
    }

-- | - `cleanup` (default: `true`): Kill the spawned process when the parent process exits unless either:
-- |    - the spawned process is `detached`
-- |    - the parent process is terminated abruptly, for example, with SIGKILL as opposed to SIGTERM or a normal exit
-- | `- preferLocal` (default: `Nothing`): When `Just`, includes and prefers locally-installed `node_modules/.bin` binaries
-- |   when looking for a binary to execute. In short, if you `npm install foo``, you can run `execa "foo"`.
-- |   `localDir` (if `Nothing`, `Process.cwd` is used) - Preferred path to find locally installed binaries in
-- |   `execPath` (if `Nothing`, `Process.execPath` is used) - Path to the Node.js executable to use in child processes. 
-- |      This can be either an absolute path or a path relative to the `localDir` option.
-- | - `stripFinalNewline` - (default: `true`). If enabled, trims the newline character of `stdout`/`stderr` (e.g. `/(?:/r/n)|\r|\n$/`
-- | - `extendEnv` (default: `true`) - Extends the child process' `env` with `Process.env`
-- | - `argv0` - see Node docs
-- | - `input` - When defined, the input is piped into the child's `stdin` and then `stdin` is `end`ed.
-- | - `stdioExtra` - Append any other `stdio` values to the array.
-- |    The `stdio` array used is always `["pipe", "pipe", "pipe", "ipc"] <> fromMaybe [] options.stdioExtra`
-- | - `detached` - see Node docs
-- | - `uid` - see Node docs
-- | - `gid` - see Node docs
-- | - `shell` - see Node docs. The Boolean variant is not supported
-- | - `timeout` - the amount of time to wait before killing the child process with the given kill signal
-- | - `maxBuffer` - the amount of buffer space available to `stdout`/`stderr`.
-- |    If more data is written to their buffers, child process will error with a max buffer size exceeded error.
-- | - `encoding` (default: `Just UTF8`) - the encoding to use to decode `stdout`/`stderr` to a String
-- | - `windowsVerbatimArguments` - see Node docs
-- | - `windowsHide` - see Node docs
-- | - `windowsEnableCmdEcho` (default: `true`) - Enables the `\q` flag when using the `cmd` shell. See https://github.com/nodejs/node/issues/27120
-- |    This goes against the Windows' defaults but makes the `stdout`/`stderr` behavior more consistent across different operating systems.
type ExecaSyncOptions =
  -- execa options
  { cleanup :: Maybe Boolean
  , preferLocal :: Maybe { localDir :: Maybe String, execPath :: Maybe String }
  , stripFinalNewline :: Maybe Boolean
  , extendEnv :: Maybe Boolean
  , cwd :: Maybe String
  , env :: Maybe (Object String)
  , argv0 :: Maybe String
  , input :: Maybe ImmutableBuffer
  , stdioExtra :: Maybe (Array Foreign)
  , detached :: Maybe Boolean
  , uid :: Maybe Int
  , gid :: Maybe Int
  , shell :: Maybe String
  , timeout :: Maybe { milliseconds :: Number, killSignal :: Either Int String }
  , maxBuffer :: Maybe Number
  , encoding :: Maybe Encoding
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  -- cross spawn options
  , windowsEnableCmdEcho :: Maybe Boolean
  }

defaultExecaSyncOptions :: ExecaSyncOptions
defaultExecaSyncOptions =
  { cleanup: Nothing
  , preferLocal: Nothing
  , stripFinalNewline: Nothing
  , extendEnv: Nothing
  , cwd: Nothing
  , env: Nothing
  , argv0: Nothing
  , input: Nothing
  , stdioExtra: Nothing
  , detached: Nothing
  , uid: Nothing
  , gid: Nothing
  , shell: Nothing
  , timeout: Nothing
  , maxBuffer: Nothing
  , encoding: Nothing
  , windowsVerbatimArguments: Nothing
  , windowsHide: Nothing
  , windowsEnableCmdEcho: Nothing
  }

-- | Replacement for `childProcess.spawnSync`. Override the default options
-- | using record update syntax. If defaults are good enough, just use `identity`.
-- | ```
-- | execaSync "jq" [ "-M", "--" ] (_ 
-- |    { input = Just $ ImmutableBuffer.fromString UTF8 """{ "json": 0, "array": ["my json"] }"""
-- |    })
-- |
-- | execaSync "jq" [ "-M", "path/to/some/file.json" ] identity
-- | ```
execaSync :: String -> Array String -> (ExecaSyncOptions -> ExecaSyncOptions) -> Effect (Either ExecaError ExecaSyncResult)
execaSync file args buildOptions = do
  let options = buildOptions defaultExecaSyncOptions
  parsed <- handleArguments file args $ Record.delete (Proxy :: _ "input") options
  let
    command = joinCommand file args
    escapedCommand = getEscapedCommand file args
  result <- spawnSync parsed.file parsed.args
    { cwd: Just parsed.options.cwd
    , input: options.input
    , argv0: parsed.options.argv0
    , stdioExtra: Just parsed.options.stdioExtra
    , env: Just parsed.options.env
    , uid: parsed.options.uid
    , gid: parsed.options.gid
    , timeout: parsed.options.timeout
    , killSignal: parsed.options.killSignal
    , maxBuffer: Just parsed.options.maxBuffer
    , shell: parsed.options.shell
    , windowsVerbatimArguments: Just parsed.options.windowsVerbatimArguments
    , windowsHide: Just parsed.options.windowsHide
    }
  let
    stripOption = fromMaybe true options.stripFinalNewline
    encoding = fromMaybe defaultOptions.encoding options.encoding
    bufferToString = ImmutableBuffer.toString encoding
  stdout' <- bufferToString <$> handleOutput { stripFinalNewline: stripOption } result.stdout
  stderr' <- bufferToString <$> handleOutput { stripFinalNewline: stripOption } result.stderr
  let
    resultError = toMaybe result.error
    resultSignal = map fromKillSignal $ toMaybe result.signal
    hasNonZeroExit = case toMaybe result.status of
      Just n | n /= 0 -> true
      _ -> false
  if isJust resultError || hasNonZeroExit || isJust resultSignal then
    pure $ Left $ mkError
      { command
      , escapedCommand
      , stdout: stdout'
      , stderr: stderr'
      , stdinErr: Nothing
      , stdoutErr: Nothing
      , stderrErr: Nothing
      , error: resultError
      , signal: resultSignal
      , exitCode: toMaybe result.status
      , parsed
      , timedOut: Just "ETIMEDOUT" == (map _.code resultError)
      , isCanceled: false
      , killed: isJust resultSignal
      }
  else do
    pure $ Right
      { command
      , escapedCommand
      , stdout: stdout'
      , stderr: stderr'
      , exitCode: 0
      }

type ExecaSyncResult =
  { command :: String
  , escapedCommand :: String
  , exitCode :: Int
  , stdout :: String
  , stderr :: String
  }

handleOutput :: forall r. { stripFinalNewline :: Boolean | r } -> ImmutableBuffer -> Effect ImmutableBuffer
handleOutput options value
  | options.stripFinalNewline =
      unsafeThaw value >>= stripFinalNewlineBuf
  | otherwise = pure value

joinCommand :: String -> Array String -> String
joinCommand file args = file <> " " <> Array.intercalate " " args

getEscapedCommand :: String -> Array String -> String
getEscapedCommand file args = do
  Array.intercalate " " $ map escapeArg $ Array.cons file args
  where
  escapeArg arg
    | test noEscapeRegex arg = arg
    | otherwise = "\"" <> (Regex.replace doubleQuotesregex ("\\" <> "\"") arg) <> "\""

data SpawnResult
  = ExitCode Int
  | Killed (Either Int String)
  | SpawnError ChildProcessError
  | StdinError Error
  | TimedOut (Either Int String)

_ExitCode :: Prism' SpawnResult Int
_ExitCode = prism ExitCode case _ of
  ExitCode i -> Right i
  other -> Left other

_Killed :: Prism' SpawnResult (Either Int String)
_Killed = prism Killed case _ of
  Killed sig -> Right sig
  other -> Left other

_SpawnError :: Prism' SpawnResult ChildProcessError
_SpawnError = prism SpawnError case _ of
  SpawnError a -> Right a
  other -> Left other

_StdinError :: Prism' SpawnResult Error
_StdinError = prism StdinError case _ of
  StdinError a -> Right a
  other -> Left other

_TimedOut :: Prism' SpawnResult (Either Int String)
_TimedOut = prism TimedOut case _ of
  TimedOut a -> Right a
  other -> Left other

-- | `/^[\w.-]+$/`
noEscapeRegex ∷ Regex
noEscapeRegex = unsafeRegex """^[\w.-]+$""" noFlags

-- | `/"/g`
doubleQuotesregex ∷ Regex
doubleQuotesregex = unsafeRegex "\"" global

spawnedKill
  :: EffectFn3
       (EffectFn1 KillSignal Boolean)
       (Nullable KillSignal)
       (Nullable Milliseconds)
       Boolean
spawnedKill = mkEffectFn3 \killFn numOrStringSignal forceKillAfterTimeout -> do
  let
    signal = case toMaybe numOrStringSignal of
      Nothing -> Right "SIGTERM"
      Just numOrStr -> fromKillSignal numOrStr
  killSignalSucceeded <- runEffectFn1 killFn $ either intKillSignal stringKillSignal signal
  let
    mbTimeout = do
      guard $ isSigTerm signal
      guard killSignalSucceeded
      toMaybe forceKillAfterTimeout
  for_ mbTimeout \(Milliseconds timeout) -> do
    t <- runEffectFn2 setTimeoutImpl (floor timeout) do
      void $ runEffectFn1 killFn $ stringKillSignal "SIGKILL"
    t.unref
  pure killSignalSucceeded
  where
  isSigTerm :: Either Int String -> Boolean
  isSigTerm = case _ of
    Left i -> maybe false (eq "SIGTERM" <<< String.toUpper <<< _.name) $ Map.lookup i signals.byNumber
    Right s -> eq "SIGTERM" $ String.toUpper s

foreign import monkeyPatchKill
  :: EffectFn2
       ChildProcess
       ( EffectFn3
           (EffectFn1 KillSignal Boolean)
           (Nullable KillSignal)
           (Nullable Milliseconds)
           Boolean
       )
       Unit

foreign import setTimeoutImpl :: EffectFn2 Int (Effect Unit) { unref :: Effect Unit }

type ExecaRunOptions =
  -- execa options
  { cleanup :: Boolean
  , stdioExtra :: Array Foreign
  , stripFinalNewline :: Boolean
  , encoding :: Encoding
  -- child process spawn options:
  , cwd :: String
  , env :: Object String -- 
  , argv0 :: Maybe String
  , detached :: Boolean
  , uid :: Maybe Int
  , gid :: Maybe Int
  , shell :: Maybe String
  , timeout :: Maybe Number
  , killSignal :: Maybe (Either Int String)
  , timeoutWithKillSignal :: Maybe { milliseconds :: Number, killSignal :: Either Int String }
  , maxBuffer :: Number
  , windowsVerbatimArguments :: Boolean
  , windowsHide :: Boolean
  }

type ExecaError =
  { originalMessage :: Maybe String
  , message :: String
  , shortMessage :: String
  , escapedCommand :: String
  , exitCode :: Maybe Int
  , signal :: Maybe (Either Int String)
  , signalDescription :: Maybe String
  , stdout :: String
  , stderr :: String
  , failed :: Boolean
  , timedOut :: Boolean
  , isCanceled :: Boolean
  , killed :: Boolean
  }

mkError
  :: { stdout :: String
     , stderr :: String
     , error :: Maybe ChildProcessError
     , stdinErr :: Maybe Exception.Error
     , stdoutErr :: Maybe Exception.Error
     , stderrErr :: Maybe Exception.Error
     , signal :: Maybe (Either Int String)
     , exitCode :: Maybe Int
     , command :: String
     , escapedCommand :: String
     , parsed :: { file :: String, args :: Array String, options :: ExecaRunOptions, parsed :: CrossSpawnConfig }
     , timedOut :: Boolean
     , isCanceled :: Boolean
     , killed :: Boolean
     }
  -> ExecaError
mkError r =
  { originalMessage: (r.error >>= _.message >>> toMaybe) <|> (map Exception.message $ r.stdinErr <|> r.stdoutErr <|> r.stderrErr)
  , message
  , shortMessage
  , escapedCommand: r.escapedCommand
  , exitCode: r.exitCode
  , signal: r.signal
  , signalDescription
  , stdout: r.stdout
  , stderr: r.stderr
  , failed: true
  , timedOut: r.timedOut
  , isCanceled: r.isCanceled
  , killed: r.killed && not r.timedOut
  }
  where
  signalDescription = r.signal >>= case _ of
    Left i -> map _.description $ Map.lookup i signals.byNumber
    Right s -> map _.description $ Object.lookup s signals.byString
  errorCode = map _.code r.error
  prefix
    | r.timedOut
    , Just timeout <- r.parsed.options.timeout =
        "timed out after " <> show timeout <> "milliseconds"
    | r.isCanceled =
        "was canceled"
    | Just code <- errorCode =
        "failed with " <> code
    | Just signal' <- r.signal
    , Just description <- signalDescription =
        "was killed with " <> show signal' <> " (" <> description <> ")"
    | Just exit <- r.exitCode =
        "failed with exit code " <> show exit
    | Just err <- r.stdinErr =
        "had error in `stdin`: " <> Exception.message err
    | Just err <- r.stdoutErr =
        "had error in `stdout`: " <> Exception.message err
    | Just err <- r.stderrErr =
        "had error in `stderr`: " <> Exception.message err
    | otherwise =
        "failed"
  execaMessage = "Command " <> prefix <> ": " <> r.command
  shortMessage = execaMessage <> (maybe "" (append "\n") $ (toMaybe <<< _.message) =<< r.error)
  message = Array.intercalate "\n"
    [ shortMessage
    , r.stderr
    , r.stdout
    ]

-- | Replacement for `childProcess.exec`. Override the default options
-- | using record update syntax. If defaults are good enough, just use `identity`.
-- | ```
-- | execaCommand "git checkout -b my-branch"
-- |    { cwd = Just $ Path.concat [ "some", "other", "directory"]
-- |    })
-- |
-- | execaCommand "git checkout -b my-branch" identity
-- | ```
execaCommand :: String -> (ExecaOptions -> ExecaOptions) -> Aff ExecaProcess
execaCommand s buildOptions = do
  case parseCommand s of
    Just { file, args } ->
      execa file args buildOptions
    Nothing ->
      liftEffect $ throw $ "Command " <> show s <> " could not be parsed into `{ file :: String, args :: Array String }` value."

-- | Replacement for `childProcess.execSync`. Override the default options
-- | using record update syntax. If defaults are good enough, just use `identity`.
-- | Note: this will throw an error if the string does not contain
-- | a valid command.
-- | ```
-- | execaCommandSync "git checkout -b my-branch" (_
-- |    { cwd = Just $ Path.concat [ "some", "other", "directory"]
-- |    })
-- |
-- | execaCommandSync "git checkout -b my-branch" identity
-- | ```
execaCommandSync :: String -> (ExecaSyncOptions -> ExecaSyncOptions) -> Effect (Either ExecaError ExecaSyncResult)
execaCommandSync s buildOptions = do
  case parseCommand s of
    Just { file, args } ->
      execaSync file args buildOptions
    Nothing ->
      liftEffect $ throw $ "Command " <> show s <> " could not be parsed into `{ file :: String, args :: Array String }` value."

{-

### Child Process Module Start ###

-}

-- | A handle for inter-process communication (IPC).
foreign import data Handle :: Type

-- | Opaque type returned by `spawn`.
-- | Needed as input for most methods in this module.
foreign import data ChildProcess :: Type

channel
  :: ChildProcess
  -> Effect (Maybe { ref :: Effect Unit, unref :: Effect Unit })
channel cp = toMaybe <$> runEffectFn1 channelImpl cp

foreign import channelImpl :: EffectFn1 (ChildProcess) (Nullable { ref :: Effect Unit, unref :: Effect Unit })

-- | Indicates whether it is still possible to send and receive
-- | messages from the child process.
connected
  :: ChildProcess
  -> Effect Boolean
connected cp = runEffectFn1 connectedImpl cp

foreign import connectedImpl :: EffectFn1 (ChildProcess) Boolean

-- | Closes the IPC channel between parent and child.
disconnect :: ChildProcess -> Effect Unit
disconnect cp = runEffectFn1 disconnectImpl cp

foreign import disconnectImpl :: EffectFn1 (ChildProcess) Unit

exitCode :: ChildProcess -> Effect (Maybe Int)
exitCode cp = toMaybe <$> runEffectFn1 exitCodeImpl cp

foreign import exitCodeImpl :: EffectFn1 (ChildProcess) (Nullable Int)

-- | Same as `kill' SIGTERM`
kill :: ChildProcess -> Effect Boolean
kill = kill' (stringKillSignal "SIGTERM")

kill' :: KillSignal -> ChildProcess -> Effect Boolean
kill' sig cp = kill'' sig Nothing cp

kill'' :: KillSignal -> Maybe Milliseconds -> ChildProcess -> Effect Boolean
kill'' sig forceKillAfterTimeout cp = runEffectFn3 killImpl cp sig (toNullable forceKillAfterTimeout)

-- | Send a signal to a child process. In the same way as the
-- | [unix kill(2) system call](https://linux.die.net/man/2/kill),
-- | sending a signal to a child process won't necessarily kill it.
-- |
-- | The resulting effects of this function depend on the process
-- | and the signal. They can vary from system to system.
-- | The child process might emit an `"error"` event if the signal
-- | could not be delivered.
-- |
-- | If `forceKillAfterTimeout` is defined and
-- | the kill signal was successful, `childProcess.kill "SIGKILL"`
-- | will be called once the timeout is reached.
foreign import killImpl :: EffectFn3 (ChildProcess) KillSignal (Nullable Milliseconds) Boolean

pidExists :: ChildProcess -> Effect Boolean
pidExists cp = runEffectFn1 pidExistsImpl cp

foreign import pidExistsImpl :: EffectFn1 (ChildProcess) Boolean

killed :: ChildProcess -> Effect Boolean
killed cp = runEffectFn1 killedImpl cp

foreign import killedImpl :: EffectFn1 (ChildProcess) Boolean

-- | The process ID of a child process. Note that if the process has already
-- | exited, another process may have taken the same ID, so be careful!
pid :: ChildProcess -> Effect (Maybe Pid)
pid cp = toMaybe <$> runEffectFn1 pidImpl cp

foreign import pidImpl :: EffectFn1 (ChildProcess) (Nullable Pid)

ref :: ChildProcess -> Effect Unit
ref cp = runEffectFn1 refImpl cp

foreign import refImpl :: EffectFn1 (ChildProcess) Unit

unref :: ChildProcess -> Effect Unit
unref cp = runEffectFn1 unrefImpl cp

foreign import unrefImpl :: EffectFn1 (ChildProcess) Unit

type SendOptions =
  { keepOpen :: Maybe Boolean
  }

type JsSendOptions =
  { keepOpen :: Boolean
  }

-- | Send messages to the (`nodejs`) child process.
-- |
-- | See the [node documentation](https://nodejs.org/api/child_process.html#child_process_subprocess_send_message_sendhandle_options_callback)
-- | for in-depth documentation.
send
  :: ChildProcess
  -> Foreign
  -> Handle
  -> (SendOptions -> SendOptions)
  -> Effect Unit
  -> Effect Boolean
send cp msg handle buildOptions cb = runEffectFn5 sendImpl cp msg handle jsOptions cb
  where
  options = buildOptions { keepOpen: Nothing }
  jsOptions = { keepOpen: fromMaybe undefined options.keepOpen }

foreign import sendImpl :: EffectFn5 (ChildProcess) (Foreign) (Handle) (JsSendOptions) (Effect Unit) (Boolean)

signalCode
  :: ChildProcess
  -> Effect (Maybe String)
signalCode cp = map toMaybe $ runEffectFn1 signalCodeImpl cp

foreign import signalCodeImpl :: EffectFn1 (ChildProcess) (Nullable String)

foreign import spawnArgs :: ChildProcess -> Array String

foreign import spawnFile :: ChildProcess -> String

-- | The standard input stream of a child process.
foreign import stdin :: ChildProcess -> Writable ()

stdio :: ChildProcess -> Effect (Array Foreign)
stdio cp = runEffectFn1 stdioImpl cp

foreign import stdioImpl :: EffectFn1 (ChildProcess) (Array Foreign)

-- | The standard output stream of a child process.
foreign import stdout :: ChildProcess -> Readable ()

-- | The standard error stream of a child process.
foreign import stderr :: ChildProcess -> Readable ()

-- | Handle the `"close"` signal.
onClose :: ChildProcess -> (Maybe Int -> Maybe String -> Effect Unit) -> Effect Unit
onClose cp cb = runEffectFn2 onCloseImpl cp $ mkEffectFn2 \a b -> cb (toMaybe a) (toMaybe b)

foreign import onCloseImpl :: EffectFn2 (ChildProcess) (EffectFn2 (Nullable Int) (Nullable String) Unit) (Unit)

-- | Handle the `"disconnect"` signal.
onDisconnect :: ChildProcess -> Effect Unit -> Effect Unit
onDisconnect cp cb = runEffectFn2 onDisconnectImpl cp cb

foreign import onDisconnectImpl :: EffectFn2 (ChildProcess) (Effect Unit) (Unit)

-- | Handle the `"error"` signal.
onError :: ChildProcess -> (ChildProcessError -> Effect Unit) -> Effect Unit
onError cp cb = runEffectFn2 onErrorImpl cp $ mkEffectFn1 cb

foreign import onErrorImpl :: EffectFn2 (ChildProcess) (EffectFn1 ChildProcessError Unit) (Unit)

onExit :: ChildProcess -> (Maybe Int -> Maybe KillSignal -> Effect Unit) -> Effect Unit
onExit cp cb = runEffectFn2 onExitImpl cp $ mkEffectFn2 \e s ->
  cb (toMaybe e) (toMaybe s)

foreign import onExitImpl :: EffectFn2 (ChildProcess) (EffectFn2 (Nullable Int) (Nullable KillSignal) Unit) (Unit)

-- | Handle the `"message"` signal.
onMessage :: ChildProcess -> (Foreign -> Maybe Handle -> Effect Unit) -> Effect Unit
onMessage cp cb = runEffectFn2 onMessageImpl cp $ mkEffectFn2 \a b -> cb a (toMaybe b)

foreign import onMessageImpl :: EffectFn2 (ChildProcess) (EffectFn2 Foreign (Nullable Handle) Unit) (Unit)

onSpawn :: ChildProcess -> Effect Unit -> Effect Unit
onSpawn cp cb = runEffectFn2 onSpawnImpl cp cb

foreign import onSpawnImpl :: EffectFn2 (ChildProcess) (Effect Unit) Unit

-- | either Int or String
foreign import data KillSignal :: Type

toKillSignal :: Either Int String -> KillSignal
toKillSignal = either intKillSignal stringKillSignal

intKillSignal :: Int -> KillSignal
intKillSignal = unsafeCoerce

stringKillSignal :: String -> KillSignal
stringKillSignal = unsafeCoerce

fromKillSignal :: KillSignal -> Either Int String
fromKillSignal ks = do
  let
    ksFor :: Foreign
    ksFor = unsafeCoerce ks
    renderError errs = unsafeCrashWith
      $ append "Unexpected kill signal. Value should be String or Int but got these errors: "
      $ Foldable.intercalate "; "
      $ map renderForeignError errs

  either renderError identity $ runExcept $ (Left <$> readInt ksFor) <|> (Right <$> readString ksFor)

data SerializationOption
  = SerializeJson
  | SerializeAdvanced

derive instance Eq SerializationOption
derive instance Generic SerializationOption _
instance Show SerializationOption where
  show x = genericShow x

toJsSerialization :: SerializationOption -> String
toJsSerialization = case _ of
  SerializeJson -> "json"
  SerializeAdvanced -> "advanced"

-- Note: `signal` option intentionally not supported.
type SpawnOptions =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , argv0 :: Maybe String
  , stdioExtra :: Maybe (Array Foreign)
  , detached :: Maybe Boolean
  , uid :: Maybe Int
  , gid :: Maybe Int
  , serialization :: Maybe SerializationOption
  , shell :: Maybe String
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  }

-- Note: due to overwriting the `kill` function, so that
-- it takes another argument, passing `timeout` and `killSignal`
-- to the child process is problematic as it will call `kill`
-- without that additional argument and fail. 
-- I found that evern changing the type to `Nullable`
-- and keeping these args did not fix the issue.
-- Since we already implement a timeout in `execa`,
-- the args seems unnecessary for at least `spawn`.
type JsSpawnOptions =
  { cwd :: String
  , env :: Object String
  , argv0 :: String
  , stdio :: Array Foreign
  , detached :: Boolean
  , uid :: Int
  , gid :: Int
  , serialization :: String
  , shell :: String
  , windowsVerbatimArguments :: Boolean
  , windowsHide :: Boolean
  }

spawn
  :: String
  -> Array String
  -> SpawnOptions
  -> Effect ChildProcess
spawn cmd args options = do
  runEffectFn3 spawnImpl cmd args
    { cwd: fromMaybe undefined options.cwd
    , env: fromMaybe undefined options.env
    , argv0: fromMaybe undefined options.argv0
    , detached: fromMaybe undefined options.detached
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , serialization: maybe undefined toJsSerialization options.serialization
    , stdio: [ pipe, pipe, pipe, ipc ] <> fromMaybe [] options.stdioExtra
    , shell: fromMaybe undefined options.shell
    , windowsHide: fromMaybe undefined options.windowsHide
    , windowsVerbatimArguments: fromMaybe undefined options.windowsVerbatimArguments
    }
  where
  pipe = unsafeToForeign "pipe"
  ipc = unsafeToForeign "ipc"

foreign import spawnImpl
  :: EffectFn3
       String
       (Array String)
       (JsSpawnOptions)
       (ChildProcess)

type SpawnSyncOptions =
  { cwd :: Maybe String
  , input :: Maybe ImmutableBuffer
  , argv0 :: Maybe String
  , stdioExtra :: Maybe (Array Foreign)
  , env :: Maybe (Object String)
  , uid :: Maybe Int
  , gid :: Maybe Int
  , timeout :: Maybe Number
  , killSignal :: Maybe (Either Int String)
  , maxBuffer :: Maybe Number
  , shell :: Maybe String
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  }

type JsSpawnSyncOptions =
  { cwd :: String
  , argv0 :: String
  , input :: ImmutableBuffer
  , stdio :: Array Foreign
  , env :: Object String
  , uid :: Int
  , gid :: Int
  , timeout :: Number
  , killSignal :: KillSignal
  , maxBuffer :: Number
  , encoding :: String
  , shell :: String
  , windowsVerbatimArguments :: Boolean
  , windowsHide :: Boolean
  }

type JsSpawnSyncResult =
  { pid :: Pid
  , output :: Array Foreign
  , stdout :: ImmutableBuffer
  , stderr :: ImmutableBuffer
  , status :: Nullable Int
  , signal :: Nullable KillSignal
  , error :: Nullable ChildProcessError
  }

spawnSync :: String -> Array String -> SpawnSyncOptions -> Effect JsSpawnSyncResult
spawnSync file args options = do
  runEffectFn3 spawnSyncImpl file args jsOptions
  where
  pipe = unsafeToForeign "pipe"
  ignore = unsafeToForeign "ignore"
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , argv0: fromMaybe undefined options.argv0
    , input: fromMaybe undefined options.input
    , stdio: [ pipe, pipe, pipe, ignore ] <> fromMaybe [] options.stdioExtra
    , env: fromMaybe undefined options.env
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , timeout: fromMaybe undefined options.timeout
    , killSignal: fromMaybe undefined $ map (either intKillSignal stringKillSignal) options.killSignal
    , maxBuffer: fromMaybe undefined options.maxBuffer
    , encoding: "buffer" -- force stdout/stderr in callback to be Buffers
    , shell: fromMaybe undefined options.shell
    , windowsHide: fromMaybe undefined options.windowsHide
    , windowsVerbatimArguments: fromMaybe undefined options.windowsVerbatimArguments
    }

foreign import spawnSyncImpl
  :: EffectFn3
       String
       (Array String)
       (JsSpawnSyncOptions)
       JsSpawnSyncResult

foreign import undefined :: forall a. a

-- | An error which occurred inside a child process.
type ChildProcessError =
  { code :: String
  , errno :: String
  , syscall :: String
  , message :: Nullable String
  }

foreign import bufferToReadStream :: ImmutableBuffer -> Readable ()
