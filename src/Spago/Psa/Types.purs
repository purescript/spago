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
  , PathInfo
  , Position
  , PsaEnv
  , Suggestion
  , Lines
  , psaResultCodec
  , psaErrorCodec
  , compareByLocation
  ) where

import Prelude

import Data.Codec.JSON as CJ
import Data.Codec.JSON as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Spago.Core.Config as Core
import Spago.Path (LocalPath, RootPath)
import Spago.Purs (PursEnv)

type PsaEnv a = PursEnv (rootPath :: RootPath | a)

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
    Src s -> "(Src " <> show s <> ")"
    Lib s -> "(Lib " <> show s <> ")"
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
  , shouldShowError :: ErrorCode -> String -> Boolean
  , shouldPromoteWarningToError :: Boolean
  }

type PathInfo =
  { path :: PsaPath
  , shouldShowError :: ErrorCode -> String -> Boolean
  , shouldPromoteWarningToError :: Boolean
  }

type PsaArgs =
  { jsonErrors :: Boolean
  , color :: Boolean
  , statVerbosity :: Core.StatVerbosity
  , decisions :: Array (LocalPath -> Maybe PathDecision)
  }

type PsaOutputOptions =
  { statVerbosity :: Maybe Core.StatVerbosity
  , strict :: Maybe Boolean
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

psaResultCodec :: CJ.Codec PsaResult
psaResultCodec = CJ.named "PsaResult" $ CJ.Record.object
  { warnings: CJ.array psaErrorCodec
  , errors: CJ.array psaErrorCodec
  }

psaErrorCodec :: CJ.Codec PsaError
psaErrorCodec = CJ.named "PsaError" $ CJ.Record.object
  { moduleName: CJ.Common.nullable CJ.string
  , errorCode: CJ.string
  , errorLink: CJ.string
  , message: CJ.string
  , filename: CJ.Common.nullable CJ.string
  , position: CJ.Common.nullable positionCodec
  , suggestion: CJ.Common.nullable suggestionCodec
  }

positionCodec :: CJ.Codec Position
positionCodec = CJ.named "Position" $ CJ.Record.object
  { startLine: CJ.int
  , startColumn: CJ.int
  , endLine: CJ.int
  , endColumn: CJ.int
  }

suggestionCodec :: CJ.Codec Suggestion
suggestionCodec = CJ.named "Suggestion" $ CJ.Record.object
  { replacement: CJ.string
  , replaceRange: CJ.Common.nullable positionCodec
  }
