module Data.Bifunctor where

import Control.Category (identity)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

-- | A `Bifunctor` is a `Functor` from the pair category `(Type, Type)` to `Type`.
-- |
-- | A type constructor with two type arguments can be made into a `Bifunctor` if
-- | both of its type arguments are covariant.
-- |
-- | The `bimap` function maps a pair of functions over the two type arguments
-- | of the bifunctor.
-- |
-- | Laws:
-- |
-- | - Identity: `bimap identity identity == identity`
-- | - Composition: `bimap f1 g1 <<< bimap f2 g2 == bimap (f1 <<< f2) (g1 <<< g2)`
-- |
class Bifunctor f where
  bimap :: forall a b c d. (a -> b) -> (c -> d) -> f a c -> f b d

-- | Map a function over the first type argument of a `Bifunctor`.
lmap :: forall f a b c. Bifunctor f => (a -> b) -> f a c -> f b c
lmap f = bimap f identity

-- | Map a function over the second type arguments of a `Bifunctor`.
rmap :: forall f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap = bimap identity

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left l) = Left (f l)
  bimap _ g (Right r) = Right (g r)

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)

instance bifunctorConst :: Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
