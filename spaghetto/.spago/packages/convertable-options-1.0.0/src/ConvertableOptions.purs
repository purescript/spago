module ConvertableOptions where

import Prelude

import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

class ConvertOptionsWithDefaults t defaults provided all | t -> defaults all where
  convertOptionsWithDefaults :: t -> defaults -> provided -> all

instance convertOptionsWithDefaultsRecord ::
  ( ConvertOptions t { | provided } provided'
  , Defaults { | defaults } provided' { | all }
  ) =>
  ConvertOptionsWithDefaults t { | defaults } { | provided } { | all } where
  convertOptionsWithDefaults t def =
    defaults def <<< convertOptions t

class ConvertOptions t i o | t -> o where
  convertOptions :: t -> i -> o

class ConvertOption t (p :: Symbol) i o | t p -> o where
  convertOption :: t -> Proxy p -> i -> o

class ConvertRecordOptions t (rl :: RowList Type) i o | t rl -> o where
  convertRecordOptions :: t -> Proxy rl -> i -> o

instance convertRecordOptionsNil :: ConvertRecordOptions t RowList.Nil { | r } (Builder {} {}) where
  convertRecordOptions _ _ _ = identity

instance convertRecordOptionsCons ::
  ( ConvertRecordOptions t rest { | r } (Builder { | i } { | o' })
  , ConvertOption t sym a b
  , Row.Cons sym a r' r
  , Row.Cons sym b o' o
  , Row.Lacks sym o'
  , IsSymbol sym
  ) =>
  ConvertRecordOptions t (RowList.Cons sym a rest) { | r } (Builder { | i }  { | o }) where
  convertRecordOptions t _ r =
    Builder.insert (Proxy :: _ sym) (convertOption t (Proxy :: _ sym) (Record.get (Proxy :: _ sym) r))
      <<< convertRecordOptions t (Proxy :: _ rest) r

instance convertOptionsRecord ::
  ( RowToList i rl
  , ConvertRecordOptions t rl { | i } (Builder {} { | o })
  ) =>
  ConvertOptions t { | i } { | o } where
  convertOptions t i = Builder.buildFromScratch $ convertRecordOptions t (Proxy :: _ rl) i

class Defaults defaults provided all | defaults provided -> all where
  defaults :: defaults -> provided -> all

instance defaultsRecord ::
  ( Row.Union provided defaults all'
  , Row.Nub all' all
  ) =>
  Defaults { | defaults } { | provided } { | all } where
  defaults = flip Record.merge
