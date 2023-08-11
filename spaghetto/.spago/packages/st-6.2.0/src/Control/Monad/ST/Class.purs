module Control.Monad.ST.Class where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Global as Global
import Effect (Effect)

class Monad m <= MonadST s m | m -> s where
  liftST :: ST s ~> m

instance monadSTEffect :: MonadST Global Effect where
  liftST = Global.toEffect

instance monadSTST :: MonadST s (ST s) where
  liftST = identity
