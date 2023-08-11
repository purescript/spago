module ExitCodes (ExitCode(..)) where

import Prelude
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Maybe (Maybe(..))

-- | References:
-- | 1. Advanced Bash Scripting Guide, Appendix E
-- | 2. /usr/include/sysexits.h
-- | 3. man 7 signal

data ExitCode
  = Success
  | Error
  | MisuseOfShellBuiltins
  | CLIUsageError
  | DataFormatError
  | CannotOpenInput
  | AddresseeUnknown
  | HostNameUnknown
  | ServiceUnavailable
  | InternalSoftwareError
  | SystemError
  | CriticalOSFileMissing
  | CannotCreateOutputFile
  | IOError
  | TemporaryFailure
  | RemoteError
  | PermissionDenied
  | ConfigurationError
  | CannotExecute
  | CommandNotFound
  | InvalidExitArgument
  | SIGHUP
  | SIGINT
  | SIGQUIT
  | SIGILL
  | SIGABRT
  | SIGFPE
  | SIGKILL
  | SIGSEGV
  | SIGPIPE
  | SIGALRM
  | SIGTERM

derive instance eqExitCode :: Eq ExitCode
derive instance ordExitCode :: Ord ExitCode
instance showExitCode :: Show ExitCode where
  show = case _ of
    Success -> "Success"
    Error -> "Error"
    MisuseOfShellBuiltins -> "MisuseOfShellBuiltins"
    CLIUsageError -> "CLIUsageError"
    DataFormatError -> "DataFormatError"
    CannotOpenInput -> "CannotOpenInput"
    AddresseeUnknown -> "AddresseeUnknown"
    HostNameUnknown -> "HostNameUnknown"
    ServiceUnavailable -> "ServiceUnavailable"
    InternalSoftwareError -> "InternalSoftwareError"
    SystemError -> "SystemError"
    CriticalOSFileMissing -> "CriticalOSFileMissing"
    CannotCreateOutputFile -> "CannotCreateOutputFile"
    IOError -> "IOError"
    TemporaryFailure -> "TemporaryFailure"
    RemoteError -> "RemoteError"
    PermissionDenied -> "PermissionDenied"
    ConfigurationError -> "ConfigurationError"
    CannotExecute -> "CannotExecute"
    CommandNotFound -> "CommandNotFound"
    InvalidExitArgument -> "InvalidExitArgument"
    SIGHUP -> "SIGHUP"
    SIGINT -> "SIGINT"
    SIGQUIT -> "SIGQUIT"
    SIGILL -> "SIGILL"
    SIGABRT -> "SIGABRT"
    SIGFPE -> "SIGFPE"
    SIGKILL -> "SIGKILL"
    SIGSEGV -> "SIGSEGV"
    SIGPIPE -> "SIGPIPE"
    SIGALRM -> "SIGALRM"
    SIGTERM -> "SIGTERM"

instance boundedExitCode :: Bounded ExitCode where
  bottom = Success
  top = SIGTERM

instance enumExitCode :: Enum ExitCode where
  succ Success = Just Error
  succ Error = Just MisuseOfShellBuiltins
  succ MisuseOfShellBuiltins = Just CLIUsageError
  succ CLIUsageError = Just DataFormatError
  succ DataFormatError = Just CannotOpenInput
  succ CannotOpenInput = Just AddresseeUnknown
  succ AddresseeUnknown = Just HostNameUnknown
  succ HostNameUnknown = Just ServiceUnavailable
  succ ServiceUnavailable = Just InternalSoftwareError
  succ InternalSoftwareError = Just SystemError
  succ SystemError = Just CriticalOSFileMissing
  succ CriticalOSFileMissing = Just CannotCreateOutputFile
  succ CannotCreateOutputFile = Just IOError
  succ IOError = Just TemporaryFailure
  succ TemporaryFailure = Just RemoteError
  succ RemoteError = Just PermissionDenied
  succ PermissionDenied = Just ConfigurationError
  succ ConfigurationError = Just CannotExecute
  succ CannotExecute = Just CommandNotFound
  succ CommandNotFound = Just InvalidExitArgument
  succ InvalidExitArgument = Just SIGHUP
  succ SIGHUP = Just SIGINT
  succ SIGINT = Just SIGQUIT
  succ SIGQUIT = Just SIGILL
  succ SIGILL = Just SIGABRT
  succ SIGABRT = Just SIGFPE
  succ SIGFPE = Just SIGKILL
  succ SIGKILL = Just SIGSEGV
  succ SIGSEGV = Just SIGPIPE
  succ SIGPIPE = Just SIGALRM
  succ SIGALRM = Just SIGTERM
  succ SIGTERM = Nothing
  pred Success = Nothing
  pred Error = Just Success
  pred MisuseOfShellBuiltins = Just Error
  pred CLIUsageError = Just MisuseOfShellBuiltins
  pred DataFormatError = Just CLIUsageError
  pred CannotOpenInput = Just DataFormatError
  pred AddresseeUnknown = Just CannotOpenInput
  pred HostNameUnknown = Just AddresseeUnknown
  pred ServiceUnavailable = Just HostNameUnknown
  pred InternalSoftwareError = Just ServiceUnavailable
  pred SystemError = Just InternalSoftwareError
  pred CriticalOSFileMissing = Just SystemError
  pred CannotCreateOutputFile = Just CriticalOSFileMissing
  pred IOError = Just CannotCreateOutputFile
  pred TemporaryFailure = Just IOError
  pred RemoteError = Just TemporaryFailure
  pred PermissionDenied = Just RemoteError
  pred ConfigurationError = Just PermissionDenied
  pred CannotExecute = Just ConfigurationError
  pred CommandNotFound = Just CannotExecute
  pred InvalidExitArgument = Just CommandNotFound
  pred SIGHUP = Just InvalidExitArgument
  pred SIGINT = Just SIGHUP
  pred SIGQUIT = Just SIGINT
  pred SIGILL = Just SIGQUIT
  pred SIGABRT = Just SIGILL
  pred SIGFPE = Just SIGABRT
  pred SIGKILL = Just SIGFPE
  pred SIGSEGV = Just SIGKILL
  pred SIGPIPE = Just SIGSEGV
  pred SIGALRM = Just SIGPIPE
  pred SIGTERM = Just SIGALRM

instance boundedEnumExitCode :: BoundedEnum ExitCode where
  cardinality = Cardinality 32
  toEnum 0 = Just Success
  toEnum 1 = Just Error
  toEnum 2 = Just MisuseOfShellBuiltins
  toEnum 64 = Just CLIUsageError
  toEnum 65 = Just DataFormatError
  toEnum 66 = Just CannotOpenInput
  toEnum 67 = Just AddresseeUnknown
  toEnum 68 = Just HostNameUnknown
  toEnum 69 = Just ServiceUnavailable
  toEnum 70 = Just InternalSoftwareError
  toEnum 71 = Just SystemError
  toEnum 72 = Just CriticalOSFileMissing
  toEnum 73 = Just CannotCreateOutputFile
  toEnum 74 = Just IOError
  toEnum 75 = Just TemporaryFailure
  toEnum 76 = Just RemoteError
  toEnum 77 = Just PermissionDenied
  toEnum 78 = Just ConfigurationError
  toEnum 126 = Just CannotExecute
  toEnum 127 = Just CommandNotFound
  toEnum 128 = Just InvalidExitArgument
  toEnum 129 = Just SIGHUP
  toEnum 130 = Just SIGINT
  toEnum 131 = Just SIGQUIT
  toEnum 132 = Just SIGILL
  toEnum 134 = Just SIGABRT
  toEnum 136 = Just SIGFPE
  toEnum 137 = Just SIGKILL
  toEnum 139 = Just SIGSEGV
  toEnum 141 = Just SIGPIPE
  toEnum 142 = Just SIGALRM
  toEnum 143 = Just SIGTERM
  toEnum _ = Nothing
  fromEnum Success = 0
  fromEnum Error = 1
  fromEnum MisuseOfShellBuiltins = 2
  fromEnum CLIUsageError = 64
  fromEnum DataFormatError = 65
  fromEnum CannotOpenInput = 66
  fromEnum AddresseeUnknown = 67
  fromEnum HostNameUnknown = 68
  fromEnum ServiceUnavailable = 69
  fromEnum InternalSoftwareError = 70
  fromEnum SystemError = 71
  fromEnum CriticalOSFileMissing = 72
  fromEnum CannotCreateOutputFile = 73
  fromEnum IOError = 74
  fromEnum TemporaryFailure = 75
  fromEnum RemoteError = 76
  fromEnum PermissionDenied = 77
  fromEnum ConfigurationError = 78
  fromEnum CannotExecute = 126
  fromEnum CommandNotFound = 127
  fromEnum InvalidExitArgument = 128
  fromEnum SIGHUP = 128 + 1
  fromEnum SIGINT = 128 + 2
  fromEnum SIGQUIT = 128 + 3
  fromEnum SIGILL = 128 + 4
  fromEnum SIGABRT = 128 + 6
  fromEnum SIGFPE = 128 + 8
  fromEnum SIGKILL = 128 + 9
  fromEnum SIGSEGV = 128 + 11
  fromEnum SIGPIPE = 128 + 13
  fromEnum SIGALRM = 128 + 14
  fromEnum SIGTERM = 128 + 15
