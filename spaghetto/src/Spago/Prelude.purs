module Spago.Prelude
  ( module Prelude
  , module Extra
  , Spago(..)
  , runSpago
  , throwError
  , spawnFromParentWithStdin
  , parseUrl
  , parallelise
  ) where

import Prelude

import Control.Alt ((<|>)) as Extra
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Error.Class (try, catchError) as Extra
import Control.Monad.Reader (ask, asks) as Extra
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.State (StateT) as Extra
import Control.Parallel as Parallel
import Data.Array ((..)) as Extra
import Data.DateTime.Instant (Instant) as Extra
import Data.Either (Either(..), isLeft, isRight) as Extra
import Data.Filterable (partition, partitionMap) as Extra
import Data.Foldable (foldMap, for_, foldl, and, or) as Extra
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Generic.Rep (class Generic) as Extra
import Data.Identity (Identity(..)) as Extra
import Data.List (List, (:)) as Extra
import Data.Map (Map) as Extra
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe, maybe) as Extra
import Data.Newtype (class Newtype, unwrap) as Extra
import Data.Posix.Signal (Signal(..))
import Data.Set (Set) as Extra
import Data.Show.Generic (genericShow) as Extra
import Data.Traversable (for, traverse) as Extra
import Data.TraversableWithIndex (forWithIndex) as Extra
import Data.Tuple (Tuple(..)) as Extra
import Data.Tuple.Nested ((/\)) as Extra
import Effect (Effect) as Extra
import Effect.Aff (Aff, Error) as Extra
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Class (liftAff) as Extra
import Effect.Class (class MonadEffect)
import Effect.Class (liftEffect) as Extra
import Effect.Exception.Unsafe (unsafeThrow) as Extra
import Effect.Ref (Ref) as Extra
import Effect.Ref as Ref
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..)) as Extra
import Node.Path (FilePath) as Extra
import Node.Process as Process
import Node.Stream as Stream
import Spago.Log (logDebug, logError, logInfo, logSuccess, logWarn, die, LogOptions, LogEnv, toDoc, indent, indent2, output) as Extra
import Unsafe.Coerce (unsafeCoerce)

newtype Spago env a = Spago (ReaderT env Extra.Aff a)

derive instance Extra.Newtype (Spago env a) _

derive newtype instance Functor (Spago env)
derive newtype instance Apply (Spago env)
derive newtype instance Applicative (Spago env)
derive newtype instance Bind (Spago env)
derive newtype instance Monad (Spago env)
derive newtype instance MonadEffect (Spago env)
derive newtype instance MonadAff (Spago env)
derive newtype instance MonadThrow Extra.Error (Spago env)
derive newtype instance MonadError Extra.Error (Spago env)
derive newtype instance MonadAsk env (Spago env)

runSpago' :: forall a env. env -> Spago env a -> Extra.Aff a
runSpago' env (Spago m) = runReaderT m env

runSpago :: forall m a env. MonadAff m => env -> Spago env a -> m a
runSpago env a = Extra.liftAff (runSpago' env a)

throwError :: forall a m. MonadThrow Extra.Error m => String -> m a
throwError = Aff.throwError <<< Aff.error

type SpawnArgs =
  { command :: String
  , args :: Array String
  , input :: Extra.Maybe String
  , cwd :: Extra.Maybe String
  }

spawnFromParentWithStdin
  :: SpawnArgs
  -> Extra.Aff
       { stdout :: String
       , stderr :: String
       , code :: Int
       }
spawnFromParentWithStdin { command, args, input, cwd } = Aff.makeAff \k -> do
  stdoutRef <- Ref.new ""
  stderrRef <- Ref.new ""
  childProc <- ChildProcess.spawn command args (ChildProcess.defaultSpawnOptions { cwd = cwd })
    { stdio = map Extra.Just $
        [ ChildProcess.ShareStream (unsafeCoerce Process.stdin)
        , ChildProcess.ShareStream (unsafeCoerce Process.stdout)
        , ChildProcess.ShareStream (unsafeCoerce Process.stderr)
        ]
    }
  Extra.for_ input \inp -> do
    _ <- Stream.writeString (ChildProcess.stdin childProc) Extra.UTF8 inp mempty
    Stream.end (ChildProcess.stdin childProc) mempty

  -- Stream.onDataString (ChildProcess.stdout childProc) Extra.UTF8 \string ->
  --   Ref.modify_ (_ <> string) stdoutRef

  -- Stream.onDataString (ChildProcess.stderr childProc) Extra.UTF8 \string ->
  --   Ref.modify_ (_ <> string) stderrRef

  ChildProcess.onError childProc (k <<< Extra.Left <<< ChildProcess.toStandardError)

  ChildProcess.onExit childProc case _ of
    ChildProcess.Normally code
      | code > 0 -> Process.exit code
      | otherwise -> do
          stdout <- Ref.read stdoutRef
          stderr <- Ref.read stderrRef
          k (Extra.Right { stdout, stderr, code })
    ChildProcess.BySignal _ ->
      Process.exit 1

  pure $ Aff.effectCanceler do
    ChildProcess.kill SIGABRT childProc

parseUrl :: String -> Extra.Either String URL
parseUrl = runFn3 parseUrlImpl Extra.Left (Extra.Right <<< unsafeCoerce)

type URL = { href :: String }

foreign import parseUrlImpl :: forall r. Fn3 (String -> r) (String -> r) String r

parallelise :: forall env a. Array (Spago env a) -> Spago env Unit
parallelise actions = do
  env <- Extra.ask
  fibers <- Extra.liftAff $ Parallel.parSequence (map (Aff.forkAff <<< runSpago env) actions :: Array _)
  Extra.liftAff $ Extra.for_ fibers Aff.joinFiber
