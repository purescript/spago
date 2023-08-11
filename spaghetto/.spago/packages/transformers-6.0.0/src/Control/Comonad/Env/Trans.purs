-- | This module defines the environment comonad transformer, `EnvT`.

module Control.Comonad.Env.Trans where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Control.Extend (class Extend, (<<=))

import Data.FoldableWithIndex (class FoldableWithIndex, foldrWithIndex, foldlWithIndex, foldMapWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Traversable (class Traversable, class Foldable, foldl, foldr, foldMap, traverse, sequence)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Newtype (class Newtype)

-- | The environment comonad transformer.
-- |
-- | This comonad transformer extends the context of a value in the base comonad with a _global environment_ of
-- | type `e`.
-- |
-- | The `ComonadEnv` type class describes the operations supported by this comonad.
newtype EnvT :: forall k. Type -> (k -> Type) -> k -> Type
newtype EnvT e w a = EnvT (Tuple e (w a))

-- | Unwrap a value in the `EnvT` comonad.
runEnvT :: forall e w a. EnvT e w a -> Tuple e (w a)
runEnvT (EnvT x) = x

-- | Change the environment type in an `EnvT` context.
withEnvT :: forall e1 e2 w a. (e1 -> e2) -> EnvT e1 w a -> EnvT e2 w a
withEnvT f (EnvT (Tuple e x)) = EnvT $ Tuple (f e) x

-- | Change the underlying comonad and data type in an `EnvT` context.
mapEnvT :: forall e w1 w2 a b. (w1 a -> w2 b) -> EnvT e w1 a -> EnvT e w2 b
mapEnvT f (EnvT (Tuple e x)) = EnvT $ Tuple e (f x)

derive instance newtypeEnvT :: Newtype (EnvT e w a) _

instance functorEnvT :: Functor w => Functor (EnvT e w) where
  map f (EnvT (Tuple e x)) = EnvT $ Tuple e (f <$> x)

instance extendEnvT :: Extend w => Extend (EnvT e w) where
  extend f (EnvT (Tuple e x)) = EnvT $ Tuple e (f <$> ((Tuple e >>> EnvT) <<= x))

instance comonadEnvT :: Comonad w => Comonad (EnvT e w) where
  extract (EnvT (Tuple _ x)) = extract x

instance comonadTransEnvT :: ComonadTrans (EnvT e) where
  lower (EnvT (Tuple _ x)) = x

instance foldableEnvT :: Foldable f => Foldable (EnvT e f) where
  foldl fn a (EnvT (Tuple _ x)) = foldl fn a x
  foldr fn a (EnvT (Tuple _ x)) = foldr fn a x
  foldMap fn (EnvT (Tuple _ x)) = foldMap fn x

instance traversableEnvT :: Traversable f => Traversable (EnvT e f) where
  sequence (EnvT (Tuple a x)) = EnvT <$> Tuple a <$> sequence x
  traverse f (EnvT (Tuple a x)) = EnvT <$> Tuple a <$> traverse f x

instance functorWithIndexEnvT :: FunctorWithIndex i w => FunctorWithIndex i (EnvT e w) where
  mapWithIndex f (EnvT (Tuple e x)) = EnvT $ Tuple e (mapWithIndex f x)

instance foldableWithIndexEnvT :: FoldableWithIndex i w => FoldableWithIndex i (EnvT e w) where
  foldlWithIndex f a (EnvT (Tuple _ x)) = foldlWithIndex f a x
  foldrWithIndex f a (EnvT (Tuple _ x)) = foldrWithIndex f a x
  foldMapWithIndex f (EnvT (Tuple _ x)) = foldMapWithIndex f x

instance traversableWithIndexEnvT :: TraversableWithIndex i w => TraversableWithIndex i (EnvT e w) where
  traverseWithIndex f (EnvT (Tuple e x)) = EnvT <$> Tuple e <$> traverseWithIndex f x
