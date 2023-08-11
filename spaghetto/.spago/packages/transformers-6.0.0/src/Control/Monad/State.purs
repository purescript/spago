-- | This module defines the `State` monad.

module Control.Monad.State
  ( State
  , runState
  , evalState
  , execState
  , mapState
  , withState
  , module Control.Monad.State.Class
  , module Control.Monad.State.Trans
  ) where

import Prelude

import Control.Monad.State.Class (class MonadState, get, gets, modify, modify_, put, state)
import Control.Monad.State.Trans (class MonadTrans, StateT(..), evalStateT, execStateT, lift, mapStateT, runStateT, withStateT)

import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))

-- | The `State` monad is a synonym for the `StateT` monad transformer, applied
-- | to the `Identity` monad.
type State s = StateT s Identity

-- | Run a computation in the `State` monad
runState :: forall s a. State s a -> s -> Tuple a s
runState (StateT s) = unwrap <<< s

-- | Run a computation in the `State` monad, discarding the final state
evalState :: forall s a. State s a -> s -> a
evalState (StateT m) s = case m s of Identity (Tuple a _) -> a

-- | Run a computation in the `State` monad, discarding the result
execState :: forall s a. State s a -> s -> s
execState (StateT m) s = case m s of Identity (Tuple _ s') -> s'

-- | Change the type of the result in a `State` action
mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b
mapState f = mapStateT (Identity <<< f <<< unwrap)

-- | Modify the state in a `State` action
withState :: forall s a. (s -> s) -> State s a -> State s a
withState = withStateT
