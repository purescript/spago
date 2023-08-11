module Type.Function where

-- | Polymorphic Type application
-- |
-- | For example...
-- | ```
-- | APPLY Maybe Int == Maybe $ Int == Maybe Int
-- | ```
type APPLY :: forall a b. (a -> b) -> a -> b
type APPLY f a = f a

infixr 0 type APPLY as $

-- | Reversed polymorphic Type application
-- |
-- | For example...
-- | ```
-- | FLIP Int Maybe == Int # Maybe == Maybe Int
-- | ```
type FLIP :: forall a b. a -> (a -> b) -> b
type FLIP a f = f a

infixl 1 type FLIP as #
