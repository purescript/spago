-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa where

import Spago.Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.DateTime.Instant (toDateTime)
import Data.Foldable (foldr)
import Data.Set as Set
import Effect.Exception as Exception
import Data.String as Str
import Effect.Now (now)
import Effect.Ref as Ref
import Foreign.Object as FO
import Node.Encoding as Encoding
import Node.FS.Stats as Stats
import Node.FS.Aff as FSA
import Spago.Purs as Purs
import Spago.Psa.Types (PsaOutputOptions, StatVerbosity(..), psaResultCodec, psaErrorCodec)
import Spago.Psa.Output (buildOutput)
import Spago.Psa.Printer.Default as DefaultPrinter
import Spago.Psa.Printer.Json as JsonPrinter

defaultOptions :: PsaOutputOptions
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

defaultParseOptions :: ParseOptions
defaultParseOptions =
  { showSource: true
  , stash: false
  , stashFile: ".psa-stash"
  , jsonErrors: false
  , opts: defaultOptions
  }

type ParseOptions =
  { opts :: PsaOutputOptions
  , showSource :: Boolean
  , stash :: Boolean
  , stashFile :: String
  , jsonErrors :: Boolean
  }

parseOptions
  :: Array String
  -> Effect ParseOptions
parseOptions args =
  Array.foldM parse defaultParseOptions args
  where
  parse p arg
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

    | isPrefix "--stash=" arg =
        pure p { stash = true, stashFile = Str.drop 8 arg }

    | otherwise = pure p

  isPrefix s str =
    case Str.indexOf (Str.Pattern s) str of
      Just x | x == 0 -> true
      _ -> false

psaCompile :: forall a. Set.Set FilePath -> Array String -> Array String -> Spago (Purs.PursEnv a) Unit
psaCompile globs pursArgs libDirs = psaCompile' globs pursArgs
  $ defaultParseOptions
      { opts = defaultParseOptions.opts { libDirs = libDirs }
      }

psaCompile' :: forall a. Set.Set FilePath -> Array String -> ParseOptions -> Spago (Purs.PursEnv a) Unit
psaCompile' globs pursArgs { opts, showSource, stash, stashFile, jsonErrors } = do
  stashData <-
    if stash then readStashFile stashFile
    else emptyStash

  result <- Purs.compile globs (Array.snoc pursArgs "--json-errors")
  let
    result' = case result of
      Left err -> { output: err.stdout, exitCode: err.exitCode, err: Just err }
      Right success -> { output: success.stdout, exitCode: Just success.exitCode, err: Nothing }
  arrErrorsIsEmpty <- forWithIndex (Str.split (Str.Pattern "\n") result'.output) \idx err ->
    case jsonParser err >>= CA.decode psaResultCodec >>> lmap CA.printJsonDecodeError of
      Left decodeErrMsg -> do
        logDebug $ Array.intercalate "\n"
          [ "Failed to decode PsaResult at index '" <> show idx <> "': " <> decodeErrMsg
          , "Json was: " <> err
          ]
        -- Note to reviewer:
        -- Should a stash decode error cause a non-zero exit?
        pure false
      Right out -> do
        files <- liftEffect $ Ref.new FO.empty
        let
          loadLinesImpl = if showSource then loadLines files else loadNothing
          filenames = insertFilenames (insertFilenames Set.empty out.errors) out.warnings
        merged <- mergeWarnings filenames stashData.date stashData.stash out.warnings
        when stash $ writeStashFile stashFile merged

        out' <- buildOutput loadLinesImpl opts out { warnings = merged }

        liftEffect $ if jsonErrors then JsonPrinter.print out' else DefaultPrinter.print opts out'

        pure $ FO.isEmpty out'.stats.allErrors

  if Array.all identity arrErrorsIsEmpty then do
    logSuccess "Build succeeded."
  else do
    for_ result'.err $ logDebug <<< show
    die [ "Failed to build." ]

  where
  insertFilenames = foldr \x s -> maybe s (flip Set.insert s) x.filename
  loadNothing _ _ = pure Nothing

  isEmptySpan filename pos =
    filename == ""
      || pos.startLine
      == 0
      && pos.endLine
      == 0
      && pos.startColumn
      == 0
      && pos.endColumn
      == 0

  -- TODO: Handle exceptions
  loadLines files filename pos
    | isEmptySpan filename pos = pure Nothing
    | otherwise = do
        result <- try do
          cache <- liftEffect $ FO.lookup filename <$> Ref.read files
          contents <-
            case cache of
              Just lines -> pure lines
              Nothing -> do
                lines <- liftAff $ Str.split (Str.Pattern "\n") <$> FSA.readTextFile Encoding.UTF8 filename
                liftEffect $ Ref.modify_ (FO.insert filename lines) files
                pure lines
          let source = Array.slice (pos.startLine - 1) (pos.endLine) contents
          pure $ Just source
        either (const (pure Nothing)) pure result

  decodeStash s = jsonParser s >>= CA.decode (CA.array psaErrorCodec) >>> lmap CA.printJsonDecodeError
  encodeStash s = CA.encode (CA.array psaErrorCodec) s

  emptyStash = do
    logDebug $ "Using empty stash"
    liftEffect $ { date: _, stash: [] } <$> toDateTime <$> now

  readStashFile stashFile' = do
    logDebug $ "About to read stash file: " <> stashFile'
    result <- try do
      stat <- liftAff $ FSA.stat stashFile'
      file <- liftAff $ FSA.readTextFile Encoding.UTF8 stashFile'
      case decodeStash file of
        Left err -> do
          logDebug $ "Error decoding stash file: " <> err
          emptyStash
        Right stash' -> do
          logDebug $ "Successfully decoded stash file"
          pure { date: Stats.modifiedTime stat, stash: stash' }
    case result of
      Left err -> do
        logDebug $ "Reading stash file failed: " <> Exception.message err
        emptyStash
      Right cache -> pure cache

  writeStashFile stashFile' warnings = do
    logDebug $ "Writing stash file: " <> stashFile'
    let file = stringify (encodeStash warnings)
    liftAff $ FSA.writeTextFile Encoding.UTF8 stashFile' file

  mergeWarnings filenames date old new = do
    fileStat <- liftEffect $ Ref.new FO.empty
    old' <- flip Array.filterA old \x ->
      case x.filename of
        Nothing -> pure false
        Just f ->
          if Set.member f filenames then pure false
          else do
            stat <- liftEffect $ FO.lookup f <$> Ref.read fileStat
            case stat of
              Just s -> pure s
              Nothing -> do
                s <- liftAff $ try $ (date > _) <<< Stats.modifiedTime <$> FSA.stat f
                let s' = either (const false) identity s
                _ <- liftEffect $ Ref.modify_ (FO.insert f s') fileStat
                pure s'
    pure $ old' <> new

usage :: String
usage =
  """psa - Error/Warning reporting frontend for 'purs compile'

Usage: psa [--censor-lib] [--censor-src]
           [--censor-codes=CODES] [--filter-codes=CODES]
           [--no-colors] [--no-source]
           [--is-lib=DIR] [--purs=PURS] [--stash]

Available options:
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

  CODES                  Comma-separated list of purs error codes
"""
