-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa-utils
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa.Types
  ( ErrorCode
  , ModuleName
  , Filename
  , StatVerbosity(..)
  , PsaOptions
  , PsaResult
  , PsaError
  , PsaAnnotedError
  , PsaPath(..)
  , Position
  , Suggestion
  , Lines
  , parsePsaResult
  , parsePsaError
  , encodePsaResult
  , encodePsaError
  , compareByLocation
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonNull)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Combinators as Decode.Combinators
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Foreign.Object.ST as FOST
import Unsafe.Coerce (unsafeCoerce)

type ErrorCode = String
type ModuleName = String
type Filename = String
type Lines = Array String

data StatVerbosity = NoStats | CompactStats | VerboseStats

-- | Relative files paths from the cwd, tagged as either being part of the
-- | source files or library files of a project. The `Unknown` variant exists
-- | because some psc errors are inter-module and aren't reported with a
-- | canonical file.
data PsaPath
  = Src String
  | Lib String
  | Unknown

instance eqPsaPath :: Eq PsaPath where
  eq (Src a) (Src b) = eq a b
  eq (Lib a) (Lib b) = eq a b
  eq Unknown Unknown = true
  eq _ _ = false

instance ordPsaPath :: Ord PsaPath where
  compare (Src a) (Src b) = compare a b
  compare (Src _) (Lib _) = GT
  compare (Src _) Unknown = GT
  compare (Lib _) (Src _) = LT
  compare (Lib a) (Lib b) = compare a b
  compare (Lib _) Unknown = GT
  compare Unknown Unknown = EQ
  compare Unknown _ = LT

type PsaOptions =
  { ansi :: Boolean
  , censorWarnings :: Boolean
  , censorLib :: Boolean
  , censorSrc :: Boolean
  , censorCodes :: Set ErrorCode
  , filterCodes :: Set ErrorCode
  , statVerbosity :: StatVerbosity
  , libDirs :: Array String
  , strict :: Boolean
  , cwd :: String
  }

type PsaResult =
  { warnings :: Array PsaError
  , errors :: Array PsaError
  }

type PsaError =
  { moduleName :: Maybe ModuleName
  , errorCode :: ErrorCode
  , errorLink :: String
  , suggestion :: Maybe Suggestion
  , message :: String
  , filename :: Maybe Filename
  , position :: Maybe Position
  }

type PsaAnnotedError =
  { error :: PsaError
  , path :: PsaPath
  , source :: Maybe Lines
  , position :: Maybe Position
  , message :: String
  }

type Position =
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }

type Suggestion =
  { replacement :: String
  , replaceRange :: Maybe Position
  }

compareByLocation :: PsaAnnotedError -> PsaAnnotedError -> Ordering
compareByLocation err1 err2 =
  case compare err1.path err2.path of
    EQ ->
      case err1.position, err2.position of
        Nothing, Nothing -> EQ
        Nothing, _ -> LT
        _, Nothing -> GT
        Just a, Just b ->
          compare (Tuple a.startLine a.startColumn)
            (Tuple b.startLine b.startColumn)
    x -> x

parsePsaResult :: FO.Object Json -> Either String PsaResult
parsePsaResult obj =
  { warnings: _
  , errors: _
  } <$> (obj .: "warnings" >>= traverse parsePsaError)
    <*> (obj .: "errors" >>= traverse parsePsaError)

parsePsaError :: FO.Object Json -> Either String PsaError
parsePsaError obj =
  { moduleName: _
  , errorCode: _
  , errorLink: _
  , message: _
  , filename: _
  , position: _
  , suggestion: _
  } <$> obj .: "moduleName"
    <*> obj .: "errorCode"
    <*> obj .: "errorLink"
    <*> obj .: "message"
    <*> obj .: "filename"
    <*> (obj .: "position" >>= parsePosition)
    <*> (obj .: "suggestion" >>= parseSuggestion)

parsePosition :: Maybe (FO.Object Json) -> Either String (Maybe Position)
parsePosition =
  maybe (pure Nothing) \obj -> map Just $
    { startLine: _
    , startColumn: _
    , endLine: _
    , endColumn: _
    } <$> obj .: "startLine"
      <*> obj .: "startColumn"
      <*> obj .: "endLine"
      <*> obj .: "endColumn"

parseSuggestion :: Maybe (FO.Object Json) -> Either String (Maybe Suggestion)
parseSuggestion =
  maybe (pure Nothing) \obj -> map Just $
    { replacement: _
    , replaceRange: _
    } <$> obj .: "replacement"
      <*> (obj .:? "replaceRange" >>= parsePosition)

encodePsaResult :: PsaResult -> Json
encodePsaResult res = encodeJson $ FO.runST do
  obj <- FOST.new
  _ <- FOST.poke "warnings" (encodeJson (encodePsaError <$> res.warnings)) obj
  _ <- FOST.poke "errors" (encodeJson (encodePsaError <$> res.errors)) obj
  pure obj

encodePsaError :: PsaError -> Json
encodePsaError error = encodeJson $ FO.runST do
  obj <- FOST.new
  _ <- FOST.poke "moduleName" (encodeJson error.moduleName) obj
  _ <- FOST.poke "errorCode" (encodeJson error.errorCode) obj
  _ <- FOST.poke "errorLink" (encodeJson error.errorLink) obj
  _ <- FOST.poke "message" (encodeJson error.message) obj
  _ <- FOST.poke "filename" (encodeJson error.filename) obj
  _ <- FOST.poke "position" (encodeJson (maybe jsonNull encodePosition error.position)) obj
  _ <- FOST.poke "suggestion" (encodeJson (maybe jsonNull encodeSuggestion error.suggestion)) obj
  pure obj

encodePosition :: Position -> Json
encodePosition = unsafeCoerce

encodeSuggestion :: Suggestion -> Json
encodeSuggestion suggestion = encodeJson $ FO.runST do
  obj <- FOST.new
  _ <- FOST.poke "replacement" (encodeJson suggestion.replacement) obj
  _ <- FOST.poke "replaceRange" (encodeJson (maybe jsonNull encodePosition suggestion.replaceRange)) obj
  pure obj

maybeProp :: forall a. (DecodeJson a) => FO.Object Json -> String -> Either String (Maybe a)
maybeProp obj key = maybe (Right Nothing) (lmap printJsonDecodeError <<< decodeJson) (FO.lookup key obj)

infix 7 maybeProp as .:?

getField :: forall a. (DecodeJson a) => FO.Object Json -> String -> Either String a
getField obj key = lmap printJsonDecodeError (Decode.Combinators.getField obj key)

infix 7 getField as .:
