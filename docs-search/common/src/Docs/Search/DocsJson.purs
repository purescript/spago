-- | A module containing everything that is necessary to decode `docs.json` files.
module Docs.Search.DocsJson
  ( sourceSpanCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (wrapIso)

import Language.PureScript.AST.SourcePos (SourceSpan(..), SourcePos(..))

sourceSpanCodec :: JsonCodec SourceSpan
sourceSpanCodec =
  wrapIso SourceSpan $
    CAR.object "SourceSpan"
      { start: sourcePosCodec
      , end: sourcePosCodec
      , name: CA.string
      }

sourcePosCodec :: JsonCodec SourcePos
sourcePosCodec =
  wrapIso SourcePos $
    CAR.object "SourcePos"
      { line: CA.int
      , column: CA.int
      }
