-- | This module defines functions for working with affine traversals.
-- | An `AffineTraversal` is a `Traversal` that applies to at most one element.
-- |
-- | These arise most frequently as the composition of a `Lens` with a `Prism`.
module Data.Lens.AffineTraversal
  ( affineTraversal
  , affineTraversal'
  , withAffineTraversal
  , cloneAffineTraversal
  , module Data.Lens.Types
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens.Internal.Stall (Stall(..))
import Data.Lens.Types (AffineTraversal, AffineTraversal', AnAffineTraversal, AnAffineTraversal')
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (right)
import Data.Profunctor.Strong (second, (&&&))
import Data.Tuple (Tuple(..))

affineTraversal
  :: forall s t a b
   . (s -> b -> t)
  -> (s -> Either t a)
  -> AffineTraversal s t a b
affineTraversal set pre =
  affineTraversal' (set &&& pre)

affineTraversal'
  :: forall s t a b
   . (s -> Tuple (b -> t) (Either t a))
  -> AffineTraversal s t a b
affineTraversal' to pab =
  dimap to (\(Tuple b f) -> either identity b f) (second (right pab))

withAffineTraversal
  :: forall s t a b r
   . AnAffineTraversal s t a b
  -> ((s -> b -> t) -> (s -> Either t a) -> r)
  -> r
withAffineTraversal l f = case l (Stall (const identity) Right) of
  Stall g h -> f g h

cloneAffineTraversal
  :: forall s t a b
   . AnAffineTraversal s t a b
  -> AffineTraversal s t a b
cloneAffineTraversal l =
  withAffineTraversal l \x y p ->
    affineTraversal x y p
