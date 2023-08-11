-- | This module defines the cowriter comonad transformer, `TracedT`.

module Control.Comonad.Traced.Trans where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Control.Extend (class Extend, (<<=))
import Data.Newtype (class Newtype)

-- | The cowriter comonad transformer.
-- |
-- | This comonad transformer extends the context of a value in the base comonad so that the value
-- | depends on a monoidal position of type `t`.
-- |
-- | The `ComonadTraced` type class describes the operations supported by this comonad.
newtype TracedT t w a = TracedT (w (t -> a))

-- | Unwrap a value in the `TracedT` comonad.
runTracedT :: forall w a t. TracedT t w a -> w (t -> a)
runTracedT (TracedT w) = w

derive instance newtypeTracedT :: Newtype (TracedT t w a) _

instance functorTracedT :: Functor w => Functor (TracedT t w) where
  map f (TracedT w) = TracedT ((\g t -> f $ g t) <$> w)

instance extendTracedT :: (Extend w, Semigroup t) => Extend (TracedT t w) where
  extend f (TracedT w) = TracedT ((\w' t -> f $ TracedT ((\h t' -> h $ t <> t') <$> w')) <<= w)

instance comonadTracedT :: (Comonad w, Monoid t) => Comonad (TracedT t w) where
  extract (TracedT w) = extract w mempty

instance comonadTransTracedT :: Monoid t => ComonadTrans (TracedT t) where
  lower (TracedT w) = (\f -> f mempty) <$> w
