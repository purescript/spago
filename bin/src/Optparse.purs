module Optparse where

import Prelude

import Data.Symbol (class IsSymbol)
import Options.Applicative.Types (Parser(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

-- this code is adapted from `argparse-basic`
-- it builds a record parser from a record of parsers

fromRecord
  :: forall rin rl rout
   . RowToList rin rl
  => BuildRecordArgs rl { | rin } {} { | rout }
  => { | rin }
  -> Parser { | rout }
fromRecord =
  map Builder.buildFromScratch
    <<< buildRecordArgs (Proxy :: _ rl)

class BuildRecordArgs (rl :: RowList Type) rs rin rout | rl -> rout where
  buildRecordArgs :: Proxy rl -> rs -> Parser (Builder rin rout)

instance buildRecordArgsNil :: BuildRecordArgs RowList.Nil rs rout rout where
  buildRecordArgs _ _ = NilP identity

instance buildArgsCons ::
  ( IsSymbol sym
  , Row.Cons sym (Parser a) rs' rs
  , Row.Cons sym a rin rin'
  , Row.Lacks sym rin
  , BuildRecordArgs next { | rs } { | rin' } { | rout }
  ) =>
  BuildRecordArgs (RowList.Cons sym (Parser a) next) { | rs } { | rin } { | rout } where
  buildRecordArgs _ rs =
    (\a b -> Builder.insert (Proxy :: _ sym) a >>> b)
      <$> Record.get (Proxy :: _ sym) rs
      <*> buildRecordArgs (Proxy :: _ next) rs
