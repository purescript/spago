-- | This module defines the `Cont`inuation monad.

module Control.Monad.Cont
  ( Cont
  , cont
  , runCont
  , mapCont
  , withCont
  , module Control.Monad.Cont.Trans
  , module Control.Monad.Cont.Class
  ) where

import Prelude

import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Cont.Trans (class MonadTrans, ContT(..), lift, mapContT, runContT, withContT)

import Data.Identity (Identity(..))
import Data.Newtype (unwrap)

-- | The `Cont` monad is a synonym for the `ContT` monad transformer applied to
-- | the `Identity` monad.
type Cont r = ContT r Identity

-- | Creates a computation in the `Cont` monad.
cont :: forall a r. ((a -> r) -> r) -> Cont r a
cont f = ContT (\c -> Identity (f (unwrap <<< c)))

-- | Runs a computation in the `Cont` monad.
runCont :: forall r a. ContT r Identity a -> (a -> r) -> r
runCont cc k = unwrap (runContT cc (Identity <<< k))

-- | Transform the result of a continuation-passing function.
mapCont :: forall r a. (r -> r) -> Cont r a -> Cont r a
mapCont f = mapContT (Identity <<< f <<< unwrap)

-- | Transform the continuation passed into the continuation-passing function.
withCont :: forall a b r. ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f = withContT (compose Identity <<< f <<< compose unwrap)
