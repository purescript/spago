module Type.Row
  ( module Prim.Row
  , RowApply
  , type (+)
  ) where

import Prim.Row (class Lacks, class Nub, class Cons, class Union)

-- | Type application for rows.
type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f a = f a

-- | Applies a type alias of open rows to a set of rows. The primary use case
-- | this operator is as convenient sugar for combining open rows without
-- | parentheses.
-- | ```purescript
-- | type Rows1 r = (a :: Int, b :: String | r)
-- | type Rows2 r = (c :: Boolean | r)
-- | type Rows3 r = (Rows1 + Rows2 + r)
-- | type Rows4 r = (d :: String | Rows1 + Rows2 + r)
-- | ```
infixr 0 type RowApply as +
