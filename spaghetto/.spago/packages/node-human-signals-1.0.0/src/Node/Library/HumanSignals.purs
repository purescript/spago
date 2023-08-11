module Node.Library.HumanSignals
  ( UnhandledAction(..)
  , StandardSource(..)
  , signals
  , HumanSignal
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldl)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Show.Generic (genericShow)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | The default action for a signal if it is not handled.
data UnhandledAction
  = Terminate
  | Core
  | Ignore
  | Pause
  | Unpause

derive instance Eq UnhandledAction
derive instance Generic UnhandledAction _
instance Show UnhandledAction where
  show = genericShow

-- | Which standard defined the signal
data StandardSource
  = Ansi
  | Posix
  | Bsd
  | Systemv
  | Other

derive instance Eq StandardSource
derive instance Generic StandardSource _
instance Show StandardSource where
  show = genericShow

type UnnormalizedHumanSignal =
  { name :: String
  , number :: Int
  , action :: UnhandledAction
  , description :: String
  , standard :: StandardSource
  , forced :: Boolean
  }

-- | `name` = standard name of the signal
-- | `number` = code number of the signal. Most are cross-platform, but some differ between OSes
-- | `description` = Human-friendly description for the signal
-- | `supported` = whether the current OS can handle this signal in Node.js
-- |    using `process.on(name, handler)` (see https://nodejs.org/api/process.html#process_signal_events)
-- |    Note: the list of supported signals is OS-specific. See https://github.com/ehmicky/cross-platform-node-guide/blob/main/docs/6_networking_ipc/signals.md#cross-platform-signals
-- | `forced` = whether the signal's default action cannot be prevented. This is `true` for
--      `SIGTERM`, `SIGKILL` and `SIGSTOP`.
-- | `action` = see type's docs
-- | `standard` = see type's docs
type HumanSignal =
  { name :: String
  , number :: Int
  , description :: String
  , supported :: Boolean
  , forced :: Boolean
  , action :: UnhandledAction
  , standard :: StandardSource
  }

foreign import osConstants :: { signals :: Object Int }

-- | Lookup a signal either by its name or number
signals
  :: { byName :: SignalsByName
     , byNumber :: Map Int HumanSignal
     , byString :: Object HumanSignal
     }
signals = { byName, byNumber, byString }
  where
  byNumber :: Map Int HumanSignal
  byNumber = Array.range 0 64 # flip foldl Map.empty \acc number -> do
    let
      byOsConstant = signalsArray # Array.find \{ name } ->
        eq (Just number) $ Object.lookup name osConstants.signals
      byDefinedNumber = signalsArray # Array.find (eq number <<< _.number)
    maybe acc (\sig -> Map.insert number sig acc) $ byOsConstant <|> byDefinedNumber

  signalsArray :: Array HumanSignal
  signalsArray = Object.toArrayWithKey (\_ -> identity) byString

  normalizeSignal :: UnnormalizedHumanSignal -> HumanSignal
  normalizeSignal { name, number, description, action, standard, forced } = do
    let mbConstantSignal = Object.lookup name osConstants.signals
    { name
    , number: fromMaybe number mbConstantSignal
    , description
    , supported: isJust mbConstantSignal
    , action
    , standard
    , forced
    }

  byString :: Object HumanSignal
  byString = Object.fromHomogeneous byName

  byName :: SignalsByName
  byName =
    { "SIGHUP": normalizeSignal
        { name: "SIGHUP"
        , number: 1
        , action: Terminate
        , description: "Terminal closed"
        , standard: Posix
        , forced: false
        }
    , "SIGINT": normalizeSignal
        { name: "SIGINT"
        , number: 2
        , action: Terminate
        , description: "User interruption with CTRL-C"
        , standard: Ansi
        , forced: false
        }
    , "SIGQUIT": normalizeSignal
        { name: "SIGQUIT"
        , number: 3
        , action: Core
        , description: "User interruption with CTRL-\\"
        , standard: Posix
        , forced: false
        }
    , "SIGILL": normalizeSignal
        { name: "SIGILL"
        , number: 4
        , action: Core
        , description: "Invalid machine instruction"
        , standard: Ansi
        , forced: false
        }
    , "SIGTRAP": normalizeSignal
        { name: "SIGTRAP"
        , number: 5
        , action: Core
        , description: "Debugger breakpoint"
        , standard: Posix
        , forced: false
        }
    , "SIGABRT": normalizeSignal
        { name: "SIGABRT"
        , number: 6
        , action: Core
        , description: "Aborted"
        , standard: Ansi
        , forced: false
        }
    , "SIGIOT": normalizeSignal
        { name: "SIGIOT"
        , number: 6
        , action: Core
        , description: "Aborted"
        , standard: Bsd
        , forced: false
        }
    , "SIGBUS": normalizeSignal
        { name: "SIGBUS"
        , number: 7
        , action: Core
        , description:
            "Bus error due to misaligned, non-existing address or paging error"
        , standard: Bsd
        , forced: false
        }
    , "SIGEMT": normalizeSignal
        { name: "SIGEMT"
        , number: 7
        , action: Terminate
        , description: "Command should be emulated but is not implemented"
        , standard: Other
        , forced: false
        }
    , "SIGFPE": normalizeSignal
        { name: "SIGFPE"
        , number: 8
        , action: Core
        , description: "Floating point arithmetic error"
        , standard: Ansi
        , forced: false
        }
    , "SIGKILL": normalizeSignal
        { name: "SIGKILL"
        , number: 9
        , action: Terminate
        , description: "Forced termination"
        , standard: Posix
        , forced: true
        }
    , "SIGUSR1": normalizeSignal
        { name: "SIGUSR1"
        , number: 10
        , action: Terminate
        , description: "Application-specific signal"
        , standard: Posix
        , forced: false
        }
    , "SIGSEGV": normalizeSignal
        { name: "SIGSEGV"
        , number: 11
        , action: Core
        , description: "Segmentation fault"
        , standard: Ansi
        , forced: false
        }
    , "SIGUSR2": normalizeSignal
        { name: "SIGUSR2"
        , number: 12
        , action: Terminate
        , description: "Application-specific signal"
        , standard: Posix
        , forced: false
        }
    , "SIGPIPE": normalizeSignal
        { name: "SIGPIPE"
        , number: 13
        , action: Terminate
        , description: "Broken pipe or socket"
        , standard: Posix
        , forced: false
        }
    , "SIGALRM": normalizeSignal
        { name: "SIGALRM"
        , number: 14
        , action: Terminate
        , description: "Timeout or timer"
        , standard: Posix
        , forced: false
        }
    , "SIGTERM": normalizeSignal
        { name: "SIGTERM"
        , number: 15
        , action: Terminate
        , description: "Termination"
        , standard: Ansi
        , forced: false
        }
    , "SIGSTKFLT": normalizeSignal
        { name: "SIGSTKFLT"
        , number: 16
        , action: Terminate
        , description: "Stack is empty or overflowed"
        , standard: Other
        , forced: false
        }
    , "SIGCHLD": normalizeSignal
        { name: "SIGCHLD"
        , number: 17
        , action: Ignore
        , description: "Child process terminated, paused or unpaused"
        , standard: Posix
        , forced: false
        }
    , "SIGCLD": normalizeSignal
        { name: "SIGCLD"
        , number: 17
        , action: Ignore
        , description: "Child process terminated, paused or unpaused"
        , standard: Other
        , forced: false
        }
    , "SIGCONT": normalizeSignal
        { name: "SIGCONT"
        , number: 18
        , action: Unpause
        , description: "Unpaused"
        , standard: Posix
        , forced: true
        }
    , "SIGSTOP": normalizeSignal
        { name: "SIGSTOP"
        , number: 19
        , action: Pause
        , description: "Paused"
        , standard: Posix
        , forced: true
        }
    , "SIGTSTP": normalizeSignal
        { name: "SIGTSTP"
        , number: 20
        , action: Pause
        , description: "Paused using CTRL-Z or \"suspend\""
        , standard: Posix
        , forced: false
        }
    , "SIGTTIN": normalizeSignal
        { name: "SIGTTIN"
        , number: 21
        , action: Pause
        , description: "Background process cannot read terminal input"
        , standard: Posix
        , forced: false
        }
    , "SIGBREAK": normalizeSignal
        { name: "SIGBREAK"
        , number: 21
        , action: Terminate
        , description: "User interruption with CTRL-BREAK"
        , standard: Other
        , forced: false
        }
    , "SIGTTOU": normalizeSignal
        { name: "SIGTTOU"
        , number: 22
        , action: Pause
        , description: "Background process cannot write to terminal output"
        , standard: Posix
        , forced: false
        }
    , "SIGURG": normalizeSignal
        { name: "SIGURG"
        , number: 23
        , action: Ignore
        , description: "Socket received out-of-band data"
        , standard: Bsd
        , forced: false
        }
    , "SIGXCPU": normalizeSignal
        { name: "SIGXCPU"
        , number: 24
        , action: Core
        , description: "Process timed out"
        , standard: Bsd
        , forced: false
        }
    , "SIGXFSZ": normalizeSignal
        { name: "SIGXFSZ"
        , number: 25
        , action: Core
        , description: "File too big"
        , standard: Bsd
        , forced: false
        }
    , "SIGVTALRM": normalizeSignal
        { name: "SIGVTALRM"
        , number: 26
        , action: Terminate
        , description: "Timeout or timer"
        , standard: Bsd
        , forced: false
        }
    , "SIGPROF": normalizeSignal
        { name: "SIGPROF"
        , number: 27
        , action: Terminate
        , description: "Timeout or timer"
        , standard: Bsd
        , forced: false
        }
    , "SIGWINCH": normalizeSignal
        { name: "SIGWINCH"
        , number: 28
        , action: Ignore
        , description: "Terminal window size changed"
        , standard: Bsd
        , forced: false
        }
    , "SIGIO": normalizeSignal
        { name: "SIGIO"
        , number: 29
        , action: Terminate
        , description: "I/O is available"
        , standard: Other
        , forced: false
        }
    , "SIGPOLL": normalizeSignal
        { name: "SIGPOLL"
        , number: 29
        , action: Terminate
        , description: "Watched event"
        , standard: Other
        , forced: false
        }
    , "SIGINFO": normalizeSignal
        { name: "SIGINFO"
        , number: 29
        , action: Ignore
        , description: "Request for process information"
        , standard: Other
        , forced: false
        }
    , "SIGPWR": normalizeSignal
        { name: "SIGPWR"
        , number: 30
        , action: Terminate
        , description: "Device running out of power"
        , standard: Systemv
        , forced: false
        }
    , "SIGSYS": normalizeSignal
        { name: "SIGSYS"
        , number: 31
        , action: Core
        , description: "Invalid system call"
        , standard: Other
        , forced: false
        }
    , "SIGUNUSED": normalizeSignal
        { name: "SIGUNUSED"
        , number: 31
        , action: Terminate
        , description: "Invalid system call"
        , standard: Other
        , forced: false
        }
    -- real time signals begin here --
    , "SIGRT1": normalizeSignal
        { name: "SIGRT1"
        , number: 34
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT2": normalizeSignal
        { name: "SIGRT2"
        , number: 35
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT3": normalizeSignal
        { name: "SIGRT3"
        , number: 36
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT4": normalizeSignal
        { name: "SIGRT4"
        , number: 37
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT5": normalizeSignal
        { name: "SIGRT5"
        , number: 38
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT6": normalizeSignal
        { name: "SIGRT6"
        , number: 39
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT7": normalizeSignal
        { name: "SIGRT7"
        , number: 40
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT8": normalizeSignal
        { name: "SIGRT8"
        , number: 41
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT9": normalizeSignal
        { name: "SIGRT9"
        , number: 42
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT10": normalizeSignal
        { name: "SIGRT10"
        , number: 43
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT11": normalizeSignal
        { name: "SIGRT11"
        , number: 44
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT12": normalizeSignal
        { name: "SIGRT12"
        , number: 45
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT13": normalizeSignal
        { name: "SIGRT13"
        , number: 46
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT14": normalizeSignal
        { name: "SIGRT14"
        , number: 47
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT15": normalizeSignal
        { name: "SIGRT15"
        , number: 48
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT16": normalizeSignal
        { name: "SIGRT16"
        , number: 49
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT17": normalizeSignal
        { name: "SIGRT17"
        , number: 50
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT18": normalizeSignal
        { name: "SIGRT18"
        , number: 51
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT19": normalizeSignal
        { name: "SIGRT19"
        , number: 52
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT20": normalizeSignal
        { name: "SIGRT20"
        , number: 53
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT21": normalizeSignal
        { name: "SIGRT21"
        , number: 54
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT22": normalizeSignal
        { name: "SIGRT22"
        , number: 55
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT23": normalizeSignal
        { name: "SIGRT23"
        , number: 56
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT24": normalizeSignal
        { name: "SIGRT24"
        , number: 57
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT25": normalizeSignal
        { name: "SIGRT25"
        , number: 58
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT26": normalizeSignal
        { name: "SIGRT26"
        , number: 59
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT27": normalizeSignal
        { name: "SIGRT27"
        , number: 60
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT28": normalizeSignal
        { name: "SIGRT28"
        , number: 61
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT29": normalizeSignal
        { name: "SIGRT29"
        , number: 62
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT30": normalizeSignal
        { name: "SIGRT30"
        , number: 63
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    , "SIGRT31": normalizeSignal
        { name: "SIGRT31"
        , number: 64
        , action: Terminate
        , description: "Application-specific signal (realtime)"
        , standard: Posix
        , forced: false
        }
    }

type SignalsByName =
  { "SIGABRT" :: HumanSignal
  , "SIGALRM" :: HumanSignal
  , "SIGBREAK" :: HumanSignal
  , "SIGBUS" :: HumanSignal
  , "SIGCHLD" :: HumanSignal
  , "SIGCLD" :: HumanSignal
  , "SIGCONT" :: HumanSignal
  , "SIGEMT" :: HumanSignal
  , "SIGFPE" :: HumanSignal
  , "SIGHUP" :: HumanSignal
  , "SIGILL" :: HumanSignal
  , "SIGINFO" :: HumanSignal
  , "SIGINT" :: HumanSignal
  , "SIGIO" :: HumanSignal
  , "SIGIOT" :: HumanSignal
  , "SIGKILL" :: HumanSignal
  , "SIGPIPE" :: HumanSignal
  , "SIGPOLL" :: HumanSignal
  , "SIGPROF" :: HumanSignal
  , "SIGPWR" :: HumanSignal
  , "SIGQUIT" :: HumanSignal
  , "SIGSEGV" :: HumanSignal
  , "SIGSTKFLT" :: HumanSignal
  , "SIGSTOP" :: HumanSignal
  , "SIGSYS" :: HumanSignal
  , "SIGTERM" :: HumanSignal
  , "SIGTRAP" :: HumanSignal
  , "SIGTSTP" :: HumanSignal
  , "SIGTTIN" :: HumanSignal
  , "SIGTTOU" :: HumanSignal
  , "SIGUNUSED" :: HumanSignal
  , "SIGURG" :: HumanSignal
  , "SIGUSR1" :: HumanSignal
  , "SIGUSR2" :: HumanSignal
  , "SIGVTALRM" :: HumanSignal
  , "SIGWINCH" :: HumanSignal
  , "SIGXCPU" :: HumanSignal
  , "SIGXFSZ" :: HumanSignal
  , "SIGRT1" :: HumanSignal
  , "SIGRT2" :: HumanSignal
  , "SIGRT3" :: HumanSignal
  , "SIGRT4" :: HumanSignal
  , "SIGRT5" :: HumanSignal
  , "SIGRT6" :: HumanSignal
  , "SIGRT7" :: HumanSignal
  , "SIGRT8" :: HumanSignal
  , "SIGRT9" :: HumanSignal
  , "SIGRT10" :: HumanSignal
  , "SIGRT11" :: HumanSignal
  , "SIGRT12" :: HumanSignal
  , "SIGRT13" :: HumanSignal
  , "SIGRT14" :: HumanSignal
  , "SIGRT15" :: HumanSignal
  , "SIGRT16" :: HumanSignal
  , "SIGRT17" :: HumanSignal
  , "SIGRT18" :: HumanSignal
  , "SIGRT19" :: HumanSignal
  , "SIGRT20" :: HumanSignal
  , "SIGRT21" :: HumanSignal
  , "SIGRT22" :: HumanSignal
  , "SIGRT23" :: HumanSignal
  , "SIGRT24" :: HumanSignal
  , "SIGRT25" :: HumanSignal
  , "SIGRT26" :: HumanSignal
  , "SIGRT27" :: HumanSignal
  , "SIGRT28" :: HumanSignal
  , "SIGRT29" :: HumanSignal
  , "SIGRT30" :: HumanSignal
  , "SIGRT31" :: HumanSignal
  }
