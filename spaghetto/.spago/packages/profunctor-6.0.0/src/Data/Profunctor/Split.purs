module Data.Profunctor.Split
  ( Split
  , split
  , unSplit
  , liftSplit
  , lowerSplit
  , hoistSplit
  ) where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Functor.Invariant (class Invariant, imap)
import Data.Profunctor (class Profunctor)

newtype Split f a b = Split (Exists (SplitF f a b))

data SplitF f a b x = SplitF (a -> x) (x -> b) (f x)

instance functorSplit :: Functor (Split f a) where
  map f = unSplit \g h fx -> split g (f <<< h) fx

instance profunctorSplit :: Profunctor (Split f) where
  dimap f g = unSplit \h i -> split (h <<< f) (g <<< i)

split :: forall f a b x. (a -> x) -> (x -> b) -> f x -> Split f a b
split f g fx = Split (mkExists (SplitF f g fx))

unSplit :: forall f a b r. (forall x. (a -> x) -> (x -> b) -> f x -> r) -> Split f a b -> r
unSplit f (Split e) = runExists (\(SplitF g h fx) -> f g h fx) e

liftSplit :: forall f a. f a -> Split f a a
liftSplit = split identity identity

lowerSplit :: forall f a. Invariant f => Split f a a -> f a
lowerSplit = unSplit (flip imap)

hoistSplit :: forall f g a b. (f ~> g) -> Split f a b -> Split g a b
hoistSplit nat = unSplit (\f g -> split f g <<< nat)
