-- | This module defines the `ComonadEnv` type class and its instances.

module Control.Comonad.Env.Class where

import Control.Comonad (class Comonad)
import Control.Comonad.Env.Trans (EnvT(..))

import Data.Tuple (Tuple(..), fst)

-- | The `ComonadEnv` type class represents those comonads which support a
-- | global environment that can be provided via the `ask` function.
-- |
-- | An implementation is provided for `EnvT`.
class Comonad w <= ComonadAsk e w | w -> e where
  ask :: forall a. w a -> e

-- | Get a value which depends on the environment.
asks :: forall e1 e2 w a. ComonadAsk e1 w => (e1 -> e2) -> w a -> e2
asks f x = f (ask x)

-- | The `ComonadEnv` type class extends `ComonadAsk` with a function
-- | `local f x` that allows the value of the local context to be modified for
-- | the duration of the execution of action `x`.
-- |
-- | An implementation is provided for `EnvT`.
-- |
-- | Laws:
-- |
-- | - `ask (local f x) = f (ask x)`
-- | - `extract (local _ x) = extract a`
-- | - `extend g (local f x) = extend (g <<< local f) x`
class ComonadAsk e w <= ComonadEnv e w | w -> e where
  local :: forall a. (e -> e) -> w a -> w a

instance comonadAskTuple :: ComonadAsk e (Tuple e) where
  ask = fst

instance comonadEnvTuple :: ComonadEnv e (Tuple e) where
  local f (Tuple x y) = Tuple (f x) y

instance comonadAskEnvT :: Comonad w => ComonadAsk e (EnvT e w) where
  ask (EnvT x) = fst x

instance comonadEnvEnvT :: Comonad w => ComonadEnv e (EnvT e w) where
  local f (EnvT x) = EnvT case x of
    Tuple y z -> Tuple (f y) z
