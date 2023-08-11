-- | This module defines the `Reader` monad.

module Control.Monad.Reader
  ( Reader
  , runReader
  , mapReader
  , withReader
  , module Control.Monad.Reader.Class
  , module Control.Monad.Reader.Trans
  ) where

import Prelude

import Control.Monad.Reader.Class (class MonadAsk, ask, asks, class MonadReader, local)
import Control.Monad.Reader.Trans (class MonadTrans, ReaderT(..), lift, mapReaderT, runReaderT, withReaderT)

import Data.Identity (Identity(..))
import Data.Newtype (unwrap)

-- | The `Reader` monad is a synonym for the `ReaderT` monad transformer, applied
-- | to the `Identity` monad.
type Reader r = ReaderT r Identity

-- | Run a computation in the `Reader` monad.
runReader :: forall r a. Reader r a -> r -> a
runReader (ReaderT m) = unwrap <<< m

-- | Change the type of the context in a `Reader` monad action.
withReader :: forall r1 r2 a. (r2 -> r1) -> Reader r1 a -> Reader r2 a
withReader = withReaderT

-- | Change the type of the result in a `Reader` monad action.
mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT $ Identity <<< f <<< unwrap
