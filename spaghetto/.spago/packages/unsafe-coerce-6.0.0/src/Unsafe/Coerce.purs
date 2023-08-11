
module Unsafe.Coerce
  ( unsafeCoerce
  ) where

-- | A _highly unsafe_ function, which can be used to persuade the type system that
-- | any type is the same as any other type. When using this function, it is your
-- | (that is, the caller's) responsibility to ensure that the underlying
-- | representation for both types is the same.
-- |
-- | Because this function is extraordinarily flexible, type inference
-- | can greatly suffer. It is highly recommended to define specializations of
-- | this function rather than using it as-is. For example:
-- |
-- | ```purescript
-- | fromBoolean :: Boolean -> Json
-- | fromBoolean = unsafeCoerce
-- | ```
-- |
-- | This way, you won't have any nasty surprises due to the inferred type being
-- | different to what you expected.
-- |
-- | After the v0.14.0 PureScript release, some of what was accomplished via
-- | `unsafeCoerce` can now be accomplished via `coerce` from
-- | `purescript-safe-coerce`. See that library's documentation for more
-- | context.
foreign import unsafeCoerce :: forall a b. a -> b
