module Safe.Coerce
  ( module Prim.Coerce
  , coerce
  ) where

import Prim.Coerce (class Coercible)
import Unsafe.Coerce (unsafeCoerce)

-- | Coerce a value of one type to a value of some other type, without changing
-- | its runtime representation. This function behaves identically to
-- | `unsafeCoerce` at runtime. Unlike `unsafeCoerce`, it is safe, because the
-- | `Coercible` constraint prevents any use of this function from compiling
-- | unless the compiler can prove that the two types have the same runtime
-- | representation.
-- |
-- | One application for this function is to avoid doing work that you know is a
-- | no-op because of newtypes. For example, if you have an `Array (Conj a)` and you
-- | want an `Array (Disj a)`, you could do `Data.Array.map (un Conj >>> Disj)`, but
-- | this performs an unnecessary traversal of the array, with O(n) cost.
-- | `coerce` accomplishes the same with only O(1) cost:
-- |
-- | ```purescript
-- | mapConjToDisj :: forall a. Array (Conj a) -> Array (Disj a)
-- | mapConjToDisj = coerce
-- | ```
coerce :: forall a b. Coercible a b => a -> b
coerce = unsafeCoerce
