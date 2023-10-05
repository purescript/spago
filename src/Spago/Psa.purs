-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa where

import Spago.Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Set as Set
import Data.String as Str
import Effect.Ref as Ref
import Foreign.Object as FO
import Node.Encoding as Encoding
import Node.FS.Aff as FSA
import Spago.Core.Config as Core
import Spago.Psa.Output (buildOutput)
import Spago.Psa.Printer (printDefaultOutputToErr, printJsonOutputToOut)
import Spago.Psa.Types (ErrorCode, PsaOutputOptions, psaResultCodec)
import Spago.Purs as Purs

type PsaArgs =
  { libraryDirs :: Array String
  , jsonErrors :: Boolean
  , color :: Boolean
  }

defaultParseOptions :: PsaOptions
defaultParseOptions =
  { censorBuildWarnings: Core.CensorNoWarnings
  , censorCodes: Set.empty
  , filterCodes: Set.empty
  , statVerbosity: Core.CompactStats
  , strict: false
  }

type PsaOptions =
  { censorBuildWarnings :: Core.CensorBuildWarnings
  , censorCodes :: Set ErrorCode
  , filterCodes :: Set ErrorCode
  , statVerbosity :: Core.StatVerbosity
  , strict :: Boolean
  }

toOutputOptions :: PsaArgs -> PsaOptions -> PsaOutputOptions
toOutputOptions { libraryDirs, color } options =
  { color
  , censorBuildWarnings: options.censorBuildWarnings
  , censorCodes: options.censorCodes
  , filterCodes: options.filterCodes
  , statVerbosity: options.statVerbosity
  , libraryDirs
  , strict: options.strict
  }

psaCompile :: forall a. Set.Set FilePath -> Array String -> PsaArgs -> PsaOptions -> Spago (Purs.PursEnv a) Unit
psaCompile globs pursArgs psaArgs options = do
  let outputOptions = toOutputOptions psaArgs options

  result <- Purs.compile globs (Array.snoc pursArgs "--json-errors")
  let
    result' = case result of
      Left err -> { output: err.stdout, exitCode: err.exitCode, err: Just err }
      Right success -> { output: success.stdout, exitCode: Just success.exitCode, err: Nothing }
  arrErrorsIsEmpty <- forWithIndex (Str.split (Str.Pattern "\n") result'.output) \idx err ->
    case jsonParser err >>= CA.decode psaResultCodec >>> lmap CA.printJsonDecodeError of
      Left decodeErrMsg -> do
        logWarn $ Array.intercalate "\n"
          [ "Failed to decode PsaResult at index '" <> show idx <> "': " <> decodeErrMsg
          , "Json was: " <> err
          ]
        -- If we can't decode the error, then there's likely a codec issue on Spago's side.
        -- So, this shouldn't fail the build.
        pure true
      Right out -> do
        files <- liftEffect $ Ref.new FO.empty
        out' <- buildOutput (loadLines files) outputOptions out

        liftEffect $ if psaArgs.jsonErrors then printJsonOutputToOut out' else printDefaultOutputToErr outputOptions out'

        pure $ FO.isEmpty out'.stats.allErrors

  if Array.all identity arrErrorsIsEmpty then do
    logSuccess "Build succeeded."
  else do
    for_ result'.err $ logDebug <<< show
    die [ "Failed to build." ]

  where
  isEmptySpan filename pos =
    filename == "" || pos.startLine == 0 && pos.endLine == 0 && pos.startColumn == 0 && pos.endColumn == 0

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

