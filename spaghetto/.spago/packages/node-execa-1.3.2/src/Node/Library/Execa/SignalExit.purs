-- A majority of the below code was ported from this JavaScript library
-- https://github.com/tapjs/signal-exit
-- Copyright `signal-exit` contributors
-- ISC License: https://opensource.org/license/isc/
module Node.Library.Execa.SignalExit
  ( onExit
  , onAfterExit
  ) where

import Prelude

import Data.Either (hush)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Nullable (Nullable, notNull, null, toMaybe)
import Data.Posix (Pid)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (reflectSymbol)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Exception (try)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Node.EventEmitter (CanEmit, CanHandle, EventEmitter, unsafeEmit)
import Node.EventEmitter.TypedEmitter (TypedEmitter, emit, subscribe, withEmit)
import Node.EventEmitter.TypedEmitter as TypedEmitter
import Node.Platform (Platform(..))
import Node.Process as Process
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import unsafeProcessHasProp :: EffectFn1 String Boolean
foreign import unsafeReadProcessProp :: forall a. EffectFn1 String a
foreign import unsafeWriteProcessProp :: forall a. EffectFn2 String a Unit

foreign import data ProcessEmitFn :: Type
foreign import data ProcessReallyExitFn :: Type
foreign import processCallFn :: EffectFn2 ProcessReallyExitFn (Nullable Int) Unit

foreign import processOn :: forall cb. EffectFn2 String cb Unit
foreign import processOff :: forall cb. EffectFn2 String cb Unit
foreign import processKill :: EffectFn2 Pid String Unit
foreign import processListenersLength :: EffectFn1 String Int
foreign import customProcessEmit :: EffectFn3 (EffectFn1 ProcessEmitFn Boolean) String (Nullable Int) Boolean -> EffectFn2 String (Nullable Int) Boolean
foreign import processExitCode :: Effect (Nullable Int)

isWin :: Boolean
isWin = Just Win32 == Process.platform

type Options =
  { alwaysLast :: Boolean
  }

_exit = Proxy :: Proxy "exit"
_afterexit = Proxy :: Proxy "afterexit"

onExit :: (Maybe Int -> Maybe String -> Effect Unit) -> Effect (Effect Unit)
onExit cb = onExit' cb { alwaysLast: false }

onAfterExit :: (Maybe Int -> Maybe String -> Effect Unit) -> Effect (Effect Unit)
onAfterExit cb = onExit' cb { alwaysLast: true }

-- PureScript implementation note:
-- I'm not sure what will happen if this library and
-- the original `signal-exit` JS library is used
-- in the same project.
onExit' :: (Maybe Int -> Maybe String -> Effect Unit) -> Options -> Effect (Effect Unit)
onExit' cb options = do
  { emitter } <- getGlobalRecOnProcessObject
  load
  unSubscribe <-
    if options.alwaysLast then do
      emitter # subscribe _afterexit \exitCode sig ->
        cb (toMaybe exitCode) (toMaybe sig)
    else do
      emitter # subscribe _exit \exitCode sig ->
        cb (toMaybe exitCode) (toMaybe sig)
  pure do
    unSubscribe
    exitLen <- TypedEmitter.listenersLength _exit emitter
    afterExitLen <- TypedEmitter.listenersLength _afterexit emitter
    when (exitLen == 0 && afterExitLen == 0) do
      unload
  where
  toEmitter :: forall e h r. TypedEmitter e h r -> EventEmitter e h
  toEmitter = unsafeCoerce

  unload = do
    { loadedRef
    , countRef
    , signalListenersRef
    , restoreOriginalProcessFunctions
    } <- getGlobalRecOnProcessObject
    whenM (Ref.read loadedRef) do
      Ref.write false loadedRef
      Ref.read signalListenersRef >>= traverse_ case _ of
        Nothing -> pure unit
        Just unsubscribe -> unsubscribe
      restoreOriginalProcessFunctions
      Ref.modify_ (_ - 1) countRef

  emitFn = mkEffectFn3 \event code signal -> do
    { emitter
    , emittedEventsRef
    } <- getGlobalRecOnProcessObject
    eventsAlreadyEmitted <- Ref.read emittedEventsRef
    unless (Set.member event eventsAlreadyEmitted) do
      Ref.modify_ (Set.insert event) emittedEventsRef
      map (\(_ :: Boolean) -> unit) $ unsafeEmit (toEmitter emitter) event code signal

  load = do
    { loadedRef
    , countRef
    , signalListenersRef
    } <- getGlobalRecOnProcessObject
    unlessM (Ref.read loadedRef) do
      Ref.write true loadedRef
      -- This is the number of onSignalExit's that are in play.
      -- It's important so that we can count the correct number of
      -- listeners on signals, and don't wait for the other one to
      -- handle it instead of us.
      Ref.modify_ (_ + 1) countRef
      -- PureScript implementation note:
      -- The `signals` array is not filtered to
      -- only include signals where `process.on`
      -- did not throw. Rather, we just store
      -- either a `Nothing` (it threw) or
      -- or a `Just unsubscribe` (if it did not throw).
      -- The `Just` stores the code we need to use later
      -- to remove the listener. This obviates the need
      -- for a `Object.lookup sig sigListeners` usage here
      -- that can return a `Nothing` when it's impossible
      -- for that to occur.
      signalListeners <- for signals \sig -> map hush $ try do
        let listener = mkListener sig countRef
        runEffectFn2 processOn sig listener
        pure $ void $ try do
          runEffectFn2 processOff sig listener
      Ref.write signalListeners signalListenersRef
      runEffectFn2 unsafeWriteProcessProp "emit" processEmitFn
      runEffectFn2 unsafeWriteProcessProp "reallyExit" processReallyExitFn

  -- Good
  mkListener :: String -> Ref Int -> Effect Unit
  mkListener sig countRef = do
    listenersLen <- runEffectFn1 processListenersLength sig
    count <- Ref.read countRef
    when (listenersLen == count) do
      unload
      runEffectFn3 emitFn (reflectSymbol _exit) null (notNull sig)
      runEffectFn3 emitFn (reflectSymbol _afterexit) null (notNull sig)
      -- "SIGHUP" throws an `ENOSYS` error on Windows,
      -- so use a supported signal instead
      let sig' = if isWin && sig == "SIGHUP" then "SIGINT" else sig
      runEffectFn2 processKill Process.pid sig'

  processReallyExitFn = mkEffectFn1 \(code :: Nullable Int) -> do
    { emitter
    , originalProcessReallyExit
    } <- getGlobalRecOnProcessObject
    let exitCode = fromMaybe 0 $ toMaybe code
    runEffectFn2 unsafeWriteProcessProp "exit" exitCode
    void $ withEmit $ emit _exit emitter (notNull exitCode) null
    void $ withEmit $ emit _afterexit emitter (notNull exitCode) null
    runEffectFn2 processCallFn originalProcessReallyExit (notNull exitCode)

  processEmitFn = customProcessEmit $ mkEffectFn3 \runOriginalProcessEmit ev arg -> do
    { originalProcessEmit } <- getGlobalRecOnProcessObject
    if ev == (reflectSymbol _exit) then do
      exitCode <- case toMaybe arg of
        Nothing -> processExitCode
        Just exitCode' -> do
          runEffectFn2 unsafeWriteProcessProp "exit" exitCode'
          pure $ notNull exitCode'

      ret <- runEffectFn1 runOriginalProcessEmit originalProcessEmit
      runEffectFn3 emitFn (reflectSymbol _exit) exitCode null
      runEffectFn3 emitFn (reflectSymbol _afterexit) exitCode null
      pure ret
    else do
      runEffectFn1 runOriginalProcessEmit originalProcessEmit

signalExitProp :: String
signalExitProp = "__purescript_signal_exit__"

type EmitterRows =
  ( exit :: Nullable Int -> Nullable String -> Effect Unit
  , afterexit :: Nullable Int -> Nullable String -> Effect Unit
  )

type SignalEventRecord =
  { originalProcessEmit :: ProcessEmitFn
  , originalProcessReallyExit :: ProcessReallyExitFn
  , restoreOriginalProcessFunctions :: Effect Unit
  , emitter :: TypedEmitter CanEmit CanHandle EmitterRows
  , countRef :: Ref Int
  , emittedEventsRef :: Ref (Set String)
  , loadedRef :: Ref Boolean
  , signalListenersRef :: Ref (Array (Maybe (Effect Unit)))
  }

getGlobalRecOnProcessObject :: Effect SignalEventRecord
getGlobalRecOnProcessObject =
  ifM
    (runEffectFn1 unsafeProcessHasProp signalExitProp)
    (runEffectFn1 unsafeReadProcessProp signalExitProp)
    attachRefsToProcessObject
  where
  attachRefsToProcessObject = do
    originalProcessEmit :: ProcessEmitFn <- runEffectFn1 unsafeReadProcessProp "emit"
    originalProcessReallyExit :: ProcessReallyExitFn <- runEffectFn1 unsafeReadProcessProp "reallyExit"
    let
      restoreOriginalProcessFunctions = do
        runEffectFn2 unsafeWriteProcessProp "emit" originalProcessEmit
        runEffectFn2 unsafeWriteProcessProp "reallyExit" originalProcessReallyExit

    emitter <- TypedEmitter.new (Proxy :: Proxy EmitterRows)
    TypedEmitter.setUnlimitedListeners emitter
    countRef <- Ref.new 0
    emittedEventsRef <- Ref.new Set.empty
    loadedRef <- Ref.new false
    signalListenersRef <- Ref.new ([] :: Array (Maybe (Effect Unit)))

    let
      obj =
        { originalProcessEmit
        , originalProcessReallyExit
        , restoreOriginalProcessFunctions
        , emitter
        , countRef
        , emittedEventsRef
        , loadedRef
        , signalListenersRef
        }
    runEffectFn2 unsafeWriteProcessProp signalExitProp obj
    pure obj

-- This is not the set of all possible signals.
--
-- It IS, however, the set of all signals that trigger
-- an exit on either Linux or BSD systems.  Linux is a
-- superset of the signal names supported on BSD, and
-- the unknown signals just fail to register, so we can
-- catch that easily enough.
--
-- Don"t bother with SIGKILL.  It"s uncatchable, which
-- means that we can't fire any callbacks anyway.
--
-- If a user does happen to register a handler on a non-
-- fatal signal like SIGWINCH or something, and then
-- exit, it"ll end up firing `process.emit("exit')`, so
-- the handler will be fired anyway.
--
-- SIGBUS, SIGFPE, SIGSEGV and SIGILL, when not raised
-- artificially, inherently leave the process in a
-- state from which it is not safe to try and enter JS
-- listeners.
signals :: Array String
signals = normal <> windows <> linux
  where
  normal =
    [ "SIGABRT"
    , "SIGALRM"
    , "SIGHUP"
    , "SIGINT"
    , "SIGTERM"
    ]
  windows = guard isWin
    [ "SIGVTALRM"
    , "SIGXCPU"
    , "SIGXFSZ"
    , "SIGUSR2"
    , "SIGTRAP"
    , "SIGSYS"
    , "SIGQUIT"
    , "SIGIOT"
    -- See https://github.com/tapjs/signal-exit/issues/21
    -- Should detect profiler and enable/disable accordingly.
    -- PureScript implementation note:
    -- Ideally, this would be detected automatically
    -- based on the Node args to passed to Node.
    -- For the time being, we'll just add the manual option.
    , "SIGPROF"
    ]
  linux = guard (Process.platform == Just Linux)
    [ "SIGIO"
    , "SIGPOLL"
    , "SIGPWR"
    , "SIGSTKFLT"
    , "SIGUNUSED"
    ]
