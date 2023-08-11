module Data.Symbol
  ( class IsSymbol
  , reflectSymbol
  , reifySymbol
  ) where

import Type.Proxy (Proxy(..))

-- | A class for known symbols
class IsSymbol (sym :: Symbol) where
  reflectSymbol :: Proxy sym -> String

-- local definition for use in `reifySymbol`
foreign import unsafeCoerce :: forall a b. a -> b

reifySymbol :: forall r. String -> (forall sym. IsSymbol sym => Proxy sym -> r) -> r
reifySymbol s f = coerce f { reflectSymbol: \_ -> s } Proxy
  where
  coerce
    :: (forall sym1. IsSymbol sym1 => Proxy sym1 -> r)
    -> { reflectSymbol :: Proxy "" -> String }
    -> Proxy ""
    -> r
  coerce = unsafeCoerce
