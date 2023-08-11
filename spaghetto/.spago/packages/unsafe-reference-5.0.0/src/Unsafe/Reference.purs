module Unsafe.Reference
  ( unsafeRefEq
  , reallyUnsafeRefEq
  , UnsafeRefEq(..)
  , UnsafeRefEqFallback(..)
  ) where

import Prelude

-- | Compares two values of the same type using strict (`===`) equality.
unsafeRefEq :: forall a. a -> a -> Boolean
unsafeRefEq = reallyUnsafeRefEq

-- | Compares two values of different types using strict (`===`) equality.
foreign import reallyUnsafeRefEq :: forall a b. a -> b -> Boolean

-- | The `Eq` instance is defined by `unsafeRefEq`.
newtype UnsafeRefEq a = UnsafeRefEq a

instance eqUnsafeRefEq :: Eq (UnsafeRefEq a) where
  eq (UnsafeRefEq l) (UnsafeRefEq r) = unsafeRefEq l r

-- | The `Eq` instance first checks `unsafeRefEq`, if `false` falls back to
-- | the underlying `Eq` instance.
newtype UnsafeRefEqFallback a = UnsafeRefEqFallback a

instance eqUnsafeRefEqFallback ::
  Eq a =>
  Eq (UnsafeRefEqFallback a) where
  eq (UnsafeRefEqFallback l) (UnsafeRefEqFallback r) =
    unsafeRefEq l r || l == r

