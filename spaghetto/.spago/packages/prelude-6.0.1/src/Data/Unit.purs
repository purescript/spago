module Data.Unit where

-- | The `Unit` type has a single inhabitant, called `unit`. It represents
-- | values with no computational content.
-- |
-- | `Unit` is often used, wrapped in a monadic type constructor, as the
-- | return type of a computation where only the _effects_ are important.
-- |
-- | When returning a value of type `Unit` from an FFI function, it is
-- | recommended to use `undefined`, or not return a value at all.
foreign import data Unit :: Type

-- | `unit` is the sole inhabitant of the `Unit` type.
foreign import unit :: Unit
