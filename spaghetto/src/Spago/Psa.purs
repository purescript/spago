-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Foldable (foldr, fold, for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String as Str
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Version as Version
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (catchException, throw, throwException)
import Effect.Now (now)
import Effect.Ref as Ref
import Foreign.Object as FO
import Node.ChildProcess as Child
import Node.Encoding as Encoding
import Node.FS.Stats as Stats
import Node.FS.Sync as File
import Node.Path as Path
import Node.Platform (Platform(Win32))
import Node.Process as Process
import Node.Stream as Stream
import Partial.Unsafe (unsafePartial)
import Spago.Psa (PsaOptions, StatVerbosity(..), parsePsaResult, parsePsaError, encodePsaError, output)
import Spago.Psa.Printer.Default as DefaultPrinter
import Spago.Psa.Printer.Json as JsonPrinter

foreign import version :: String

defaultOptions :: PsaOptions
defaultOptions =
  { ansi: true
  , censorWarnings: false
  , censorLib: false
  , censorSrc: false
  , censorCodes: Set.empty
  , filterCodes: Set.empty
  , statVerbosity: CompactStats
  , libDirs: []
  , strict: false
  , cwd: ""
  }

type ParseOptions =
  { extra :: Array String
  , opts :: PsaOptions
  , purs :: String
  , showSource :: Boolean
  , stash :: Boolean
  , stashFile :: String
  , jsonErrors :: Boolean
  }

parseOptions
  :: PsaOptions
  -> Array String
  -> Effect ParseOptions
parseOptions opts args =
  defaultLibDir <$>
    Array.foldM parse
      { extra: []
      , purs: "purs"
      , showSource: true
      , stash: false
      , stashFile: ".psa-stash"
      , jsonErrors: false
      , opts
      }
      args
  where
  parse p arg
    | arg == "--version" || arg == "-v" =
        Console.log version *> Process.exit 0

    | arg == "--help" || arg == "-h" =
        Console.log usage *> Process.exit 0

    | arg == "--stash" =
        pure p { stash = true }

    | arg == "--json-errors" =
        pure p { jsonErrors = true }

    | arg == "--no-source" =
        pure p { showSource = false }

    | arg == "--no-colors" || arg == "--monochrome" =
        pure p { opts = p.opts { ansi = false } }

    | arg == "--verbose-stats" =
        pure p { opts = p.opts { statVerbosity = VerboseStats } }

    | arg == "--censor-stats" =
        pure p { opts = p.opts { statVerbosity = NoStats } }

    | arg == "--strict" =
        pure p { opts = p.opts { strict = true } }

    | arg == "--censor-warnings" =
        pure p { opts = p.opts { censorWarnings = true } }

    | arg == "--censor-lib" =
        pure p { opts = p.opts { censorLib = true } }

    | arg == "--censor-src" =
        pure p { opts = p.opts { censorSrc = true } }

    | isPrefix "--censor-codes=" arg =
        pure p { opts = p.opts { censorCodes = foldr Set.insert p.opts.censorCodes (Str.split (Str.Pattern ",") (Str.drop 15 arg)) } }

    | isPrefix "--filter-codes=" arg =
        pure p { opts = p.opts { filterCodes = foldr Set.insert p.opts.filterCodes (Str.split (Str.Pattern ",") (Str.drop 15 arg)) } }

    | isPrefix "--is-lib=" arg =
        pure p { opts = p.opts { libDirs = Array.snoc p.opts.libDirs (Str.drop 9 arg) } }

    | isPrefix "--purs=" arg =
        pure p { purs = Str.drop 7 arg }

    | isPrefix "--stash=" arg =
        pure p { stash = true, stashFile = Str.drop 8 arg }

    | otherwise = pure p { extra = Array.snoc p.extra arg }

  isPrefix s str =
    case Str.indexOf (Str.Pattern s) str of
      Just x | x == 0 -> true
      _ -> false

  defaultLibDir x
    | Array.length x.opts.libDirs == 0 =
        x { opts = x.opts { libDirs = [ "bower_components", ".spago" ] } }
    | otherwise = x

main :: Effect Unit
main = void do
  cwd <- Process.cwd
  argv <- Array.drop 2 <$> Process.argv

  { extra
  , opts
  , purs
  , showSource
  , stash
  , stashFile
  , jsonErrors
  } <- parseOptions (defaultOptions { cwd = cwd }) argv

  libDirs <- traverse (Path.resolve [ cwd ] >>> map (_ <> Path.sep)) opts.libDirs
  let
    opts' = opts { libDirs = libDirs }
    args = Array.cons "compile" $ Array.cons "--json-errors" extra

  stashData <-
    if stash then readStashFile stashFile
    else emptyStash

  readPursVersion purs \pursVer -> do
    let
      outputStream =
        -- As of 0.14.0, JSON errors/warnings are written to stdout, but
        -- beforehand they were written to stderr.
        --
        -- We need to use Tuple like this because prerelease versions compare
        -- less than normal versions with the same major, minor, and patch
        -- numbers according to the semver spec.
        if Tuple (Version.major pursVer) (Version.minor pursVer) >= Tuple 0 14 then Stdout
        else Stderr
    spawn' outputStream purs args \pursResult -> do
      for_ (Str.split (Str.Pattern "\n") pursResult.output) \err ->
        case jsonParser err >>= decodeJson >>= parsePsaResult of
          Left _ -> Console.error err
          Right out -> do
            files <- Ref.new FO.empty
            let
              loadLinesImpl = if showSource then loadLines files else loadNothing
              filenames = insertFilenames (insertFilenames Set.empty out.errors) out.warnings
            merged <- mergeWarnings filenames stashData.date stashData.stash out.warnings
            when stash $ writeStashFile stashFile merged
            out' <- output loadLinesImpl opts' out { warnings = merged }
            if jsonErrors then JsonPrinter.print out'
            else DefaultPrinter.print opts' out'
            if FO.isEmpty out'.stats.allErrors then Process.exit pursResult.exitCode
            else Process.exit 1

  where
  insertFilenames = foldr \x s -> maybe s (flip Set.insert s) x.filename
  loadNothing _ _ = pure Nothing

  spawn' outputStream cmd args onExit = do
    let
      stdio =
        case outputStream of
          Stdout -> [ Just Child.Pipe, Just Child.Pipe, unsafePartial (Array.unsafeIndex Child.inherit 2) ]
          Stderr -> [ Just Child.Pipe, unsafePartial (Array.unsafeIndex Child.inherit 1), Just Child.Pipe ]

    child <- Child.spawn cmd args Child.defaultSpawnOptions { stdio = stdio }
    buffer <- Ref.new ""
    fillBuffer buffer $
      case outputStream of
        Stdout -> Child.stdout child
        Stderr -> Child.stderr child
    Child.onExit child \status ->
      case status of
        Child.Normally n -> do
          output <- Ref.read buffer
          onExit { output, exitCode: n }
        Child.BySignal s -> do
          Console.error (show s)
          Process.exit 1
    Child.onError child (retryWithCmd outputStream cmd args onExit)

  fillBuffer buffer stream =
    Stream.onDataString stream Encoding.UTF8 \chunk ->
      Ref.modify_ (_ <> chunk) buffer

  readPursVersion :: String -> (Version.Version -> Effect Unit) -> Effect Unit
  readPursVersion purs cb = do
    spawn' Stdout purs [ "--version" ] \{ output } -> do
      let verStr = Str.takeWhile (_ /= Str.codePointFromChar ' ') $ Str.trim output
      case Version.parseVersion verStr of
        Right v ->
          cb v
        Left err -> do
          throw $ fold
            [ "Unable to parse the version from `purs`. (Saw: "
            , verStr
            , "). Please check that the right executable is on your PATH."
            ]

  retryWithCmd outputStream cmd args onExit err
    | err.code == "ENOENT" = do
        -- On windows, if the executable wasn't found, try adding .cmd
        if Process.platform == Just Win32 then
          case Str.stripSuffix (Str.Pattern ".cmd") cmd of
            Nothing -> spawn' outputStream (cmd <> ".cmd") args onExit
            Just bareCmd -> throw $ "`" <> bareCmd <> "` executable not found. (nor `" <> cmd <> "`)"
        else
          throw $ "`" <> cmd <> "` executable not found."
    | otherwise =
        throwException (Child.toStandardError err)

  isEmptySpan filename pos =
    filename == "" ||
      pos.startLine == 0 && pos.endLine == 0
        && pos.startColumn == 0
        && pos.endColumn == 0

  -- TODO: Handle exceptions
  loadLines files filename pos
    | isEmptySpan filename pos = pure Nothing
    | otherwise = catchException (const (pure Nothing)) do
        cache <- FO.lookup filename <$> Ref.read files
        contents <-
          case cache of
            Just lines -> pure lines
            Nothing -> do
              lines <- Str.split (Str.Pattern "\n") <$> File.readTextFile Encoding.UTF8 filename
              Ref.modify_ (FO.insert filename lines) files
              pure lines
        let source = Array.slice (pos.startLine - 1) (pos.endLine) contents
        pure $ Just source

  decodeStash s = jsonParser s >>= decodeJson >>= traverse parsePsaError
  encodeStash s = encodeJson (encodePsaError <$> s)

  emptyStash :: forall a. Effect { date :: DateTime, stash :: Array a }
  emptyStash = { date: _, stash: [] } <$> toDateTime <$> now

  readStashFile stashFile = catchException (const emptyStash) do
    stat <- File.stat stashFile
    file <- File.readTextFile Encoding.UTF8 stashFile
    case decodeStash file of
      Left _ -> emptyStash
      Right stash -> pure { date: Stats.modifiedTime stat, stash }

  writeStashFile stashFile warnings = do
    let file = stringify (encodeStash warnings)
    File.writeTextFile Encoding.UTF8 stashFile file

  mergeWarnings filenames date old new = do
    fileStat <- Ref.new FO.empty
    old' <- flip Array.filterA old \x ->
      case x.filename of
        Nothing -> pure false
        Just f ->
          if Set.member f filenames then pure false
          else do
            stat <- FO.lookup f <$> Ref.read fileStat
            case stat of
              Just s -> pure s
              Nothing -> do
                s <- catchException (\_ -> pure false) $
                  (date > _) <<< Stats.modifiedTime <$> File.stat f
                _ <- Ref.modify_ (FO.insert f s) fileStat
                pure s
    pure $ old' <> new

-- Indicates which output stream we are interested in when spawning a child process.
data OutputStream = Stdout | Stderr

usage :: String
usage =
  """psa - Error/Warning reporting frontend for 'purs compile'

Usage: psa [--censor-lib] [--censor-src]
           [--censor-codes=CODES] [--filter-codes=CODES]
           [--no-colors] [--no-source]
           [--is-lib=DIR] [--purs=PURS] [--stash]
           PSC_OPTIONS

Available options:
  -v,--version           Show the version number
  -h,--help              Show this help text
  --verbose-stats        Show counts for each warning type
  --censor-stats         Censor warning/error summary
  --censor-warnings      Censor all warnings
  --censor-lib           Censor warnings from library sources
  --censor-src           Censor warnings from project sources
  --censor-codes=CODES   Censor specific error codes
  --filter-codes=CODES   Only show specific error codes
  --no-colors            Disable ANSI colors
  --no-source            Disable original source code printing
  --strict               Promotes src warnings to errors
  --stash                Enable persistent warnings (defaults to .psa-stash)
  --stash=FILE           Enable persistent warnings using a specific stash file
  --is-lib=DIR           Distinguishing library path (defaults to 'bower_components')
  --purs=PURS            Name of purs executable (defaults to 'purs')

  CODES                  Comma-separated list of purs error codes
  PSC_OPTIONS            Any extra options are passed to 'purs compile'
"""
