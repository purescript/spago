module Type.RowList
  ( module Prim.RowList
  , class ListToRow
  , class RowListRemove
  , class RowListSet
  , class RowListNub
  , class RowListAppend
  ) where

import Prim.Row as Row
import Prim.RowList (RowList, Cons, Nil, class RowToList)
import Type.Equality (class TypeEquals)
import Type.Data.Symbol as Symbol
import Type.Data.Boolean as Boolean

-- | Convert a RowList to a row of types.
-- | The inverse of this operation is `RowToList`.
class ListToRow :: forall k. RowList k -> Row k -> Constraint
class ListToRow list row | list -> row

instance listToRowNil
  :: ListToRow Nil ()

instance listToRowCons
  :: ( ListToRow tail tailRow
     , Row.Cons label ty tailRow row )
  => ListToRow (Cons label ty tail) row

-- | Remove all occurences of a given label from a RowList
class RowListRemove :: forall k. Symbol -> RowList k -> RowList k -> Constraint
class RowListRemove label input output | label input -> output

instance rowListRemoveNil
  :: RowListRemove label Nil Nil

instance rowListRemoveCons
  :: ( RowListRemove label tail tailOutput
     , Symbol.Equals label key eq
     , Boolean.If eq
         tailOutput
         (Cons key head tailOutput)
         output
     )
  => RowListRemove label (Cons key head tail) output

-- | Add a label to a RowList after removing other occurences.
class RowListSet :: forall k. Symbol -> k -> RowList k -> RowList k -> Constraint
class RowListSet label typ input output | label typ input -> output

instance rowListSetImpl
  :: ( TypeEquals label label'
     , TypeEquals typ typ'
     , RowListRemove label input lacking )
  => RowListSet label typ input (Cons label' typ' lacking)

-- | Remove label duplicates, keeps earlier occurrences.
class RowListNub :: forall k. RowList k -> RowList k -> Constraint
class RowListNub input output | input -> output

instance rowListNubNil
  :: RowListNub Nil Nil

instance rowListNubCons
  :: ( TypeEquals label label'
     , TypeEquals head head'
     , TypeEquals nubbed nubbed'
     , RowListRemove label tail removed
     , RowListNub removed nubbed )
  => RowListNub (Cons label head tail) (Cons label' head' nubbed')

-- Append two row lists together
class RowListAppend :: forall k. RowList k -> RowList k -> RowList k -> Constraint
class RowListAppend lhs rhs out | lhs rhs -> out

instance rowListAppendNil
  :: TypeEquals rhs out
  => RowListAppend Nil rhs out

instance rowListAppendCons
  :: ( RowListAppend tail rhs out'
     , TypeEquals (Cons label head out') out )
  => RowListAppend (Cons label head tail) rhs out
