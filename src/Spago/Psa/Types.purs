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
  , PsaArgs
  , PsaResult
  , PsaError
  , PsaAnnotedError
  , PsaPath(..)
  , PsaPathType(..)
  , PathDecision
  , PsaOutputOptions
  , WorkspacePsaOutputOptions
  , PathInfo
  , Position
  , Suggestion
  , Lines
  , psaResultCodec
  , psaErrorCodec
  , compareByLocation
  ) where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CACompat
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Data.Set.NonEmpty (NonEmptySet)
import Data.Tuple (Tuple(..))
import Spago.Core.Config as Core

type ErrorCode = String
type ModuleName = String
type Filename = String
type Lines = Array String

-- | Relative files paths from the cwd, tagged as either being part of the
-- | source files or library files of a project. The `Unknown` variant exists
-- | because some psc errors are inter-module and aren't reported with a
-- | canonical file.
data PsaPath
  = Unknown
  | Lib String
  | Src String

derive instance Eq PsaPath
derive instance Ord PsaPath
instance Show PsaPath where
  show = case _ of
    Src s -> "(Src " <> s <> ")"
    Lib s -> "(Lib " <> s <> ")"
    Unknown -> "Unknown"

data PsaPathType
  = IsLib
  | IsSrc

derive instance Eq PsaPathType
instance Show PsaPathType where
  show = case _ of
    IsLib -> "IsLib"
    IsSrc -> "IsSrc"

type PathDecision =
  { pathType :: PsaPathType
  , shouldShowError :: ErrorCode -> Boolean
  , shouldPromoteWarningToError :: Boolean
  }

type PathInfo =
  { path :: PsaPath
  , shouldShowError :: ErrorCode -> Boolean
  , shouldPromoteWarningToError :: Boolean
  }

type PsaArgs =
  { jsonErrors :: Boolean
  , color :: Boolean
  , statVerbosity :: Core.StatVerbosity
  , decisions :: Array (String -> Maybe PathDecision)
  }

type PsaOutputOptions =
  { statVerbosity :: Maybe Core.StatVerbosity
  , strict :: Maybe Boolean
  }

type WorkspacePsaOutputOptions =
  { censorLibWarnings :: Maybe Core.CensorBuildWarnings
  , censorLibCodes :: Maybe (NonEmptySet ErrorCode)
  , filterLibCodes :: Maybe (NonEmptySet ErrorCode)
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

psaResultCodec :: CA.JsonCodec PsaResult
psaResultCodec = CAR.object "PsaResult"
  { warnings: CA.array psaErrorCodec
  , errors: CA.array psaErrorCodec
  }

psaErrorCodec :: CA.JsonCodec PsaError
psaErrorCodec = CAR.object "PsaError"
  { moduleName: CACompat.maybe CA.string
  , errorCode: CA.string
  , errorLink: CA.string
  , message: CA.string
  , filename: CACompat.maybe CA.string
  , position: CACompat.maybe positionCodec
  , suggestion: CACompat.maybe suggestionCodec
  }

positionCodec :: CA.JsonCodec Position
positionCodec = CAR.object "Position"
  { startLine: CA.int
  , startColumn: CA.int
  , endLine: CA.int
  , endColumn: CA.int
  }

suggestionCodec :: CA.JsonCodec Suggestion
suggestionCodec = CAR.object "Suggestion"
  { replacement: CA.string
  , replaceRange: CACompat.maybe positionCodec
  }
