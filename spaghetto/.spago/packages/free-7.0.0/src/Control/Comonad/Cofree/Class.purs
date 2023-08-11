module Control.Comonad.Cofree.Class
  ( class ComonadCofree
  , unwrapCofree
  ) where

import Prelude

import Control.Comonad (class Comonad)
import Control.Comonad.Cofree (Cofree, tail)
import Control.Comonad.Env.Trans (EnvT(..))
import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Traced.Trans (TracedT(..))
import Data.Tuple (Tuple(..))

-- | Based on <http://hackage.haskell.org/package/free/docs/Control-Comonad-Cofree-Class.html>
class (Functor f, Comonad w) <= ComonadCofree f w | w -> f where
  unwrapCofree :: forall a. w a -> f (w a)

instance comonadCofreeCofree :: Functor f => ComonadCofree f (Cofree f) where
  unwrapCofree = tail

instance comonadCofreeEnvT :: (Functor f, ComonadCofree f w) => ComonadCofree f (EnvT e w) where
  unwrapCofree (EnvT (Tuple e wa)) = map (\x -> EnvT (Tuple e x)) (unwrapCofree wa)

instance comonadCofreeStoreT :: (Functor f, ComonadCofree f w) => ComonadCofree f (StoreT s w) where
  unwrapCofree (StoreT (Tuple wsa s)) = map (\x -> StoreT (Tuple x s)) (unwrapCofree wsa)

instance comonadCofreeTracedT :: (Functor f, ComonadCofree f w, Monoid m) => ComonadCofree f (TracedT m w) where
  unwrapCofree (TracedT wma) = map TracedT (unwrapCofree wma)
