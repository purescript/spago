-- | A module containing everything that is necessary to decode `docs.json` files.
module Docs.Search.DocsJson
  ( sourceSpanCodec
  ) where

import Prelude

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Profunctor (wrapIso)

import Language.PureScript.AST.SourcePos (SourceSpan(..), SourcePos(..))

sourceSpanCodec :: CJ.Codec SourceSpan
sourceSpanCodec =
  wrapIso SourceSpan $ CJ.named "SourceSpan" $
    CJ.Record.object
      { start: sourcePosCodec
      , end: sourcePosCodec
      , name: CJ.string
      }

sourcePosCodec :: CJ.Codec SourcePos
sourcePosCodec =
  wrapIso SourcePos $ CJ.named "SourcePos" $
    CJ.Record.object
      { line: CJ.int
      , column: CJ.int
      }
