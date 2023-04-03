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
import Data.String as Str
import Effect.Exception as Exception
import Effect.Now (now)
import Effect.Ref as Ref
import Foreign.Object as FO
import Node.Encoding as Encoding
import Node.FS.Aff as FSA
import Node.FS.Perms (permsAll)
import Node.FS.Stats as Stats
import Node.FS.Sync as FSSync
import Node.Path (dirname)
import Spago.Core.Config as Core
import Spago.Psa.Output (buildOutput)
import Spago.Psa.Printer.Default as DefaultPrinter
import Spago.Psa.Printer.Json as JsonPrinter
import Spago.Psa.Types (PsaOutputOptions, ErrorCode, psaErrorCodec, psaResultCodec)
import Spago.Purs as Purs

type PsaArgs =
  { libraryDirs :: Array String
  , jsonErrors :: Boolean
  , color :: Boolean
  }

defaultParseOptions :: PsaOptions
defaultParseOptions =
  { showSource: Core.ShowSourceCode
  , stashFile: Nothing -- ".psa-stash"
  , censorWarnings: false
  , censorLib: false
  , censorSrc: false
  , censorCodes: Set.empty
  , filterCodes: Set.empty
  , statVerbosity: Core.CompactStats
  , strict: false
  }

type PsaOptions =
  { showSource :: Core.ShowSourceCode
  , stashFile :: Maybe String
  , censorWarnings :: Boolean
  , censorLib :: Boolean
  , censorSrc :: Boolean
  , censorCodes :: Set ErrorCode
  , filterCodes :: Set ErrorCode
  , statVerbosity :: Core.StatVerbosity
  , strict :: Boolean
  }

toOutputOptions :: PsaArgs -> PsaOptions -> PsaOutputOptions
toOutputOptions { libraryDirs, color } options =
  { color
  , censorWarnings: options.censorWarnings
  , censorLib: options.censorLib
  , censorSrc: options.censorSrc
  , censorCodes: options.censorCodes
  , filterCodes: options.filterCodes
  , statVerbosity: options.statVerbosity
  , libraryDirs
  , strict: options.strict
  }

psaCompile :: forall a. Set.Set FilePath -> Array String -> PsaArgs -> PsaOptions -> Spago (Purs.PursEnv a) Unit
psaCompile globs pursArgs psaArgs options@{ showSource, stashFile } = do
  let outputOptions = toOutputOptions psaArgs options
  stashData <- case stashFile of
    Just f -> readStashFile f
    Nothing -> emptyStash

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
          loadLinesImpl = if showSource == Core.ShowSourceCode then loadLines files else loadNothing
          filenames = insertFilenames (insertFilenames Set.empty out.errors) out.warnings
        merged <- mergeWarnings filenames stashData.date stashData.stash out.warnings
        for_ stashFile \f -> writeStashFile f merged

        out' <- buildOutput loadLinesImpl outputOptions out { warnings = merged }

        liftEffect $ if psaArgs.jsonErrors then JsonPrinter.print out' else DefaultPrinter.print outputOptions out'

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
    let
      file = stringify (encodeStash warnings)
      dir = dirname stashFile'
    dirExists <- liftEffect $ FSSync.exists dir
    unless dirExists do
      liftAff $ FSA.mkdir' dir { recursive: true, mode: permsAll }
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
