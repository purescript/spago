module Control.Parallel
  ( parApply
  , parTraverse
  , parTraverse_
  , parSequence
  , parSequence_
  , parOneOf
  , parOneOfMap
  , module Control.Parallel.Class
  ) where

import Prelude

import Control.Alternative (class Alternative)
import Control.Parallel.Class (class Parallel, parallel, sequential, ParCont(..))

import Data.Foldable (class Foldable, traverse_, oneOfMap)
import Data.Traversable (class Traversable, traverse)

-- | Apply a function to an argument under a type constructor in parallel.
parApply
  :: forall f m a b
   . Parallel f m
   => m (a -> b)
   -> m a
   -> m b
parApply mf ma = sequential(apply (parallel mf) (parallel ma))

-- | Traverse a collection in parallel.
parTraverse
  :: forall f m t a b
   . Parallel f m
  => Traversable t
  => (a -> m b)
  -> t a
  -> m (t b)
parTraverse f = sequential <<< traverse (parallel <<< f)

-- | Traverse a collection in parallel, discarding any results.
parTraverse_
  :: forall f m t a b
   . Parallel f m
  => Foldable t
  => (a -> m b)
  -> t a
  -> m Unit
parTraverse_ f = sequential <<< traverse_ (parallel <<< f)

parSequence
  :: forall a t m f
   . Parallel f m
  => Traversable t
  => t (m a)
  -> m (t a)
parSequence = parTraverse identity

parSequence_
  :: forall a t m f
   . Parallel f m
  => Foldable t
  => t (m a)
  -> m Unit
parSequence_ = parTraverse_ identity

-- | Race a collection in parallel.
parOneOf
  :: forall a t m f
   . Parallel f m
  => Alternative f
  => Foldable t
  => Functor t
  => t (m a)
  -> m a
parOneOf = sequential <<< oneOfMap parallel

-- | Race a collection in parallel while mapping to some effect.
parOneOfMap
  :: forall a b t m f
   . Parallel f m
  => Alternative f
  => Foldable t
  => Functor t
  => (a -> m b)
  -> t a
  -> m b
parOneOfMap f = sequential <<< oneOfMap (parallel <<< f)
