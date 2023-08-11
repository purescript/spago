module Data.Traversable.Accum.Internal
  ( StateL(..)
  , stateL
  , StateR(..)
  , stateR
  ) where

import Prelude
import Data.Traversable.Accum (Accum)

newtype StateL s a = StateL (s -> Accum s a)

stateL :: forall s a. StateL s a -> s -> Accum s a
stateL (StateL k) = k

instance functorStateL :: Functor (StateL s) where
  map f k = StateL \s -> case stateL k s of
    { accum: s1, value: a } -> { accum: s1, value: f a }

instance applyStateL :: Apply (StateL s) where
  apply f x = StateL \s -> case stateL f s of
    { accum: s1, value: f' } -> case stateL x s1 of
      { accum: s2, value: x' } -> { accum: s2, value: f' x' }

instance applicativeStateL :: Applicative (StateL s) where
  pure a = StateL \s -> { accum: s, value: a }


newtype StateR s a = StateR (s -> Accum s a)

stateR :: forall s a. StateR s a -> s -> Accum s a
stateR (StateR k) = k

instance functorStateR :: Functor (StateR s) where
  map f k = StateR \s -> case stateR k s of
    { accum: s1, value: a } -> { accum: s1, value: f a }

instance applyStateR :: Apply (StateR s) where
  apply f x = StateR \s -> case stateR x s of
    { accum: s1, value: x' } -> case stateR f s1 of
      { accum: s2, value: f' } -> { accum: s2, value: f' x' }

instance applicativeStateR :: Applicative (StateR s) where
  pure a = StateR \s -> { accum: s, value: a }
