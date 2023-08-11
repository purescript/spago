module Data.Posix.Signal where

import Prelude
import Data.Maybe (Maybe(..))

data Signal
 = SIGABRT
 | SIGALRM
 | SIGBUS
 | SIGCHLD
 | SIGCLD
 | SIGCONT
 | SIGEMT
 | SIGFPE
 | SIGHUP
 | SIGILL
 | SIGINFO
 | SIGINT
 | SIGIO
 | SIGIOT
 | SIGKILL
 | SIGLOST
 | SIGPIPE
 | SIGPOLL
 | SIGPROF
 | SIGPWR
 | SIGQUIT
 | SIGSEGV
 | SIGSTKFLT
 | SIGSTOP
 | SIGSYS
 | SIGTERM
 | SIGTRAP
 | SIGTSTP
 | SIGTTIN
 | SIGTTOU
 | SIGUNUSED
 | SIGURG
 | SIGUSR1
 | SIGUSR2
 | SIGVTALRM
 | SIGWINCH
 | SIGXCPU
 | SIGXFSZ

-- | Convert a Signal to a String. Suitable for Node.js APIs.
toString :: Signal -> String
toString s = case s of
 SIGABRT   -> "SIGABRT"
 SIGALRM   -> "SIGALRM"
 SIGBUS    -> "SIGBUS"
 SIGCHLD   -> "SIGCHLD"
 SIGCLD    -> "SIGCLD"
 SIGCONT   -> "SIGCONT"
 SIGEMT    -> "SIGEMT"
 SIGFPE    -> "SIGFPE"
 SIGHUP    -> "SIGHUP"
 SIGILL    -> "SIGILL"
 SIGINFO   -> "SIGINFO"
 SIGINT    -> "SIGINT"
 SIGIO     -> "SIGIO"
 SIGIOT    -> "SIGIOT"
 SIGKILL   -> "SIGKILL"
 SIGLOST   -> "SIGLOST"
 SIGPIPE   -> "SIGPIPE"
 SIGPOLL   -> "SIGPOLL"
 SIGPROF   -> "SIGPROF"
 SIGPWR    -> "SIGPWR"
 SIGQUIT   -> "SIGQUIT"
 SIGSEGV   -> "SIGSEGV"
 SIGSTKFLT -> "SIGSTKFLT"
 SIGSTOP   -> "SIGSTOP"
 SIGSYS    -> "SIGSYS"
 SIGTERM   -> "SIGTERM"
 SIGTRAP   -> "SIGTRAP"
 SIGTSTP   -> "SIGTSTP"
 SIGTTIN   -> "SIGTTIN"
 SIGTTOU   -> "SIGTTOU"
 SIGUNUSED -> "SIGUNUSED"
 SIGURG    -> "SIGURG"
 SIGUSR1   -> "SIGUSR1"
 SIGUSR2   -> "SIGUSR2"
 SIGVTALRM -> "SIGVTALRM"
 SIGWINCH  -> "SIGWINCH"
 SIGXCPU   -> "SIGXCPU"
 SIGXFSZ   -> "SIGXFSZ"

-- | Try to parse a Signal from a String. Suitable for use with Node.js APIs.
-- | This function is a partial inverse of `toString`; in code, that means, for
-- | all `sig :: Signal`:
-- |
-- |   `fromString (toString sig) == Just sig`
-- |
fromString :: String -> Maybe Signal
fromString s = case s of
 "SIGABRT"   -> Just SIGABRT
 "SIGALRM"   -> Just SIGALRM
 "SIGBUS"    -> Just SIGBUS
 "SIGCHLD"   -> Just SIGCHLD
 "SIGCLD"    -> Just SIGCLD
 "SIGCONT"   -> Just SIGCONT
 "SIGEMT"    -> Just SIGEMT
 "SIGFPE"    -> Just SIGFPE
 "SIGHUP"    -> Just SIGHUP
 "SIGILL"    -> Just SIGILL
 "SIGINFO"   -> Just SIGINFO
 "SIGINT"    -> Just SIGINT
 "SIGIO"     -> Just SIGIO
 "SIGIOT"    -> Just SIGIOT
 "SIGKILL"   -> Just SIGKILL
 "SIGLOST"   -> Just SIGLOST
 "SIGPIPE"   -> Just SIGPIPE
 "SIGPOLL"   -> Just SIGPOLL
 "SIGPROF"   -> Just SIGPROF
 "SIGPWR"    -> Just SIGPWR
 "SIGQUIT"   -> Just SIGQUIT
 "SIGSEGV"   -> Just SIGSEGV
 "SIGSTKFLT" -> Just SIGSTKFLT
 "SIGSTOP"   -> Just SIGSTOP
 "SIGSYS"    -> Just SIGSYS
 "SIGTERM"   -> Just SIGTERM
 "SIGTRAP"   -> Just SIGTRAP
 "SIGTSTP"   -> Just SIGTSTP
 "SIGTTIN"   -> Just SIGTTIN
 "SIGTTOU"   -> Just SIGTTOU
 "SIGUNUSED" -> Just SIGUNUSED
 "SIGURG"    -> Just SIGURG
 "SIGUSR1"   -> Just SIGUSR1
 "SIGUSR2"   -> Just SIGUSR2
 "SIGVTALRM" -> Just SIGVTALRM
 "SIGWINCH"  -> Just SIGWINCH
 "SIGXCPU"   -> Just SIGXCPU
 "SIGXFSZ"   -> Just SIGXFSZ
 _           -> Nothing

instance showSignal :: Show Signal where
  show = toString

derive instance eqSignal :: Eq Signal
derive instance ordSignal :: Ord Signal
