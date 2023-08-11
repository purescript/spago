-- | This module defines the `Writer` monad.

module Control.Monad.Writer
  ( Writer
  , writer
  , runWriter
  , execWriter
  , mapWriter
  , module Control.Monad.Writer.Class
  , module Control.Monad.Writer.Trans
  ) where

import Prelude

import Control.Monad.Writer.Class (class MonadTell, tell, class MonadWriter, censor, listen, listens, pass)
import Control.Monad.Writer.Trans (class MonadTrans, WriterT(..), execWriterT, lift, mapWriterT, runWriterT)

import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple, snd)

-- | The `Writer` monad is a synonym for the `WriterT` monad transformer, applied
-- | to the `Identity` monad.
type Writer w = WriterT w Identity

-- | Creates a `Writer` from a result and output pair.
writer :: forall w a. Tuple a w -> Writer w a
writer = WriterT <<< pure

-- | Run a computation in the `Writer` monad
runWriter :: forall w a. Writer w a -> Tuple a w
runWriter = unwrap <<< runWriterT

-- | Run a computation in the `Writer` monad, discarding the result
execWriter :: forall w a. Writer w a -> w
execWriter m = snd (runWriter m)

-- | Change the result and accumulator types in a `Writer` monad action
mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b
mapWriter f = mapWriterT (Identity <<< f <<< unwrap)
