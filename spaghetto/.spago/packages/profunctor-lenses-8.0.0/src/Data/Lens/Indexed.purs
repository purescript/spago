module Data.Lens.Indexed where

import Prelude

import Control.Monad.State (evalState, get, modify)
import Data.Functor.Compose (Compose(..))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Setter ((%~))
import Data.Lens.Types (Indexed(..), IndexedOptic, IndexedTraversal, Optic, Traversal, wander)
import Data.Newtype (unwrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (first)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (curry, fst, snd)

-- | Converts an `IndexedOptic` to an `Optic` by forgetting indices.
unIndex
  :: forall p i s t a b
   . Profunctor p
  => IndexedOptic p i s t a b
  -> Optic p s t a b
unIndex l = l <<< Indexed <<< dimap snd identity

asIndex
  :: forall p i s t a b
   . Profunctor p
  => IndexedOptic p i s t a b
  -> Optic p s t i b
asIndex l = l <<< Indexed <<< dimap fst identity

-- | Remap the index.
reindexed
  :: forall p i j r a b
   . Profunctor p
  => (i -> j)
  -> (Indexed p i a b -> r)
  -> Indexed p j a b
  -> r
reindexed ij = (_ <<< (_Newtype %~ lcmap (first ij)))

-- | Converts a `lens`-like indexed traversal to an `IndexedTraversal`.
iwander
  :: forall i s t a b
   . (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
  -> IndexedTraversal i s t a b
iwander itr = wander (\f s -> itr (curry f) s) <<< unwrap

-- | Traverses over a `TraversableWithIndex` container.
itraversed
  :: forall i t a b
   . TraversableWithIndex i t
  => IndexedTraversal i (t a) (t b) a b
itraversed = iwander traverseWithIndex

-- | Converts a `Traversal` to an `IndexedTraversal` by using the integer
-- | positions as indices.
positions
  :: forall s t a b
   . Traversal s t a b
  -> IndexedTraversal Int s t a b
positions tr =
  iwander \f s ->
    flip evalState 0 $ unwrap $ flip unwrap s $ tr $ Star \a ->
      Compose $ (f <$> get <*> pure a) <* modify (_ + 1)
