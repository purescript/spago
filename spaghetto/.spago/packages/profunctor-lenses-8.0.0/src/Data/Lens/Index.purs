module Data.Lens.Index
  ( class Index
  , ix
  ) where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Lens (_Just, lens)
import Data.Lens.AffineTraversal (AffineTraversal', affineTraversal)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as S
import Foreign.Object as FO

-- | `Index` is a type class whose instances are optics used when:
-- | 1. The focus element might not be present.
-- | 2. You either cannot or do not want to add new elements or delete existing ones.
-- |
-- | `Array` is a typical example:
-- |
-- | ```purescript
-- | preview (ix 1) [0, 1, 2] == Just 1
-- |
-- | set (ix 1) 8888 [0, 1, 2] == [0,8888,2]
-- | ```
-- |
-- | Note the use of `preview` rather `view`. That's because the optic is
-- | a `Data.Lens.Traversal` tailored to the case where there's a single element
-- | of interest.
-- |
-- | Another common use is a `Map` that you don't want to either grow or shrink:
-- |
-- | ```purescript
-- | (set (ix "k") "new" $ Map.singleton "k" "old") == Map.singleton "k" "new"
-- |
-- | (set (ix "k") "new" $ Map.empty) == Map.empty
-- | ```
-- |
-- | Note the second line: an attempt to `set` a missing focus element
-- | leaves the original whole unchanged.
-- |
-- | If you *do* want to add or delete elements, see `Data.Lens.At`.

class Index m a b | m -> a, m -> b where
  ix :: a -> AffineTraversal' m b

instance indexFn :: Eq i => Index (i -> a) i a where
  ix i = lens (\f -> f i) \f a j -> if i == j then a else f j

instance indexMaybe :: Index (Maybe a) Unit a where
  ix _ = _Just

instance indexIdentity :: Index (Identity a) Unit a where
  ix _ = _Newtype

instance indexArray :: Index (Array a) Int a where
  ix n = affineTraversal set pre
    where
    set :: Array a -> a -> Array a
    set s b = fromMaybe s $ A.updateAt n b s

    pre :: Array a -> Either (Array a) a
    pre s = maybe (Left s) Right $ A.index s n

instance indexNonEmptyArray :: Index (NEA.NonEmptyArray a) Int a where
  ix n = affineTraversal set pre
    where
    set :: NEA.NonEmptyArray a -> a -> NEA.NonEmptyArray a
    set s b = fromMaybe s $ NEA.updateAt n b s

    pre :: NEA.NonEmptyArray a -> Either (NEA.NonEmptyArray a) a
    pre s = maybe (Left s) Right $ NEA.index s n

instance indexList :: Index (L.List a) Int a where
  ix n = affineTraversal set pre
    where
    set :: L.List a -> a -> L.List a
    set s b = fromMaybe s $ L.updateAt n b s

    pre :: L.List a -> Either (L.List a) a
    pre s = maybe (Left s) Right $ L.index s n

instance indexSet :: Ord a => Index (S.Set a) a Unit where
  ix x = affineTraversal set pre
    where
    set :: S.Set a -> Unit -> S.Set a
    set xs _ = xs

    pre :: S.Set a -> Either (S.Set a) Unit
    pre xs = if S.member x xs then Right unit else Left xs

instance indexMap :: Ord k => Index (M.Map k v) k v where
  ix k = affineTraversal set pre
    where
    set :: M.Map k v -> v -> M.Map k v
    set s b = M.update (\_ -> Just b) k s

    pre :: M.Map k v -> Either (M.Map k v) v
    pre s = maybe (Left s) Right $ M.lookup k s

instance indexForeignObject :: Index (FO.Object v) String v where
  ix k = affineTraversal set pre
    where
    set :: FO.Object v -> v -> FO.Object v
    set s b = FO.update (\_ -> Just b) k s

    pre :: FO.Object v -> Either (FO.Object v) v
    pre s = maybe (Left s) Right $ FO.lookup k s
