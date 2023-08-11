-- | This module defines a type of native homogeneous Javascript Objects.
-- |
-- | To maximize performance, Javascript objects are not wrapped,
-- | and some native code is used even when it's not necessary.

module Foreign.Object
  ( Object
  , empty
  , isEmpty
  , size
  , singleton
  , insert
  , lookup
  , toUnfoldable
  , toAscUnfoldable
  , fromFoldable
  , fromFoldableWith
  , fromFoldableWithIndex
  , fromHomogeneous
  , delete
  , pop
  , member
  , alter
  , update
  , mapWithKey
  , filterWithKey
  , filterKeys
  , filter
  , keys
  , values
  , union
  , unionWith
  , unions
  , isSubmap
  , fold
  , foldMap
  , foldM
  , foldMaybe
  , all
  , thawST
  , freezeST
  , runST
  , toArrayWithKey
  ) where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as A
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldr, for_)
import Data.FoldableWithIndex (class FoldableWithIndex, forWithIndex_)
import Data.Function.Uncurried (Fn2, runFn2, Fn4, runFn4)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Unfoldable (class Unfoldable)
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as OST
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

-- | `Object a` represents a homogeneous JS Object with values of type `a`.
foreign import data Object :: Type -> Type

type role Object representational

foreign import _copyST :: forall a b r. a -> ST r b

-- | Convert an immutable Object into a mutable Object
thawST :: forall a r. Object a -> ST r (STObject r a)
thawST = _copyST

-- | Convert a mutable Object into an immutable Object
freezeST :: forall a r. STObject r a -> ST r (Object a)
freezeST = _copyST

-- | Freeze a mutable Object, creating an immutable Object. Use this function as you would use
-- | `Control.Monad.ST.run` (from the `purescript-st` package) to freeze a mutable reference.
-- |
-- | The rank-2 type prevents the Object from escaping the scope of `runST`.
foreign import runST :: forall a. (forall r. ST r (STObject r a)) -> Object a

mutate :: forall a b. (forall r. STObject r a -> ST r b) -> Object a -> Object a
mutate f m = runST do
  s <- thawST m
  _ <- f s
  pure s

foreign import _fmapObject :: forall a b. Fn2 (Object a) (a -> b) (Object b)

instance functorObject :: Functor Object where
  map f m = runFn2 _fmapObject m f

instance functorWithIndexObject :: FunctorWithIndex String Object where
  mapWithIndex = mapWithKey

foreign import _foldM :: forall a m z. (m -> (z -> m) -> m) -> (z -> String -> a -> m) -> m -> Object a -> m

-- | Fold the keys and values of an object
fold :: forall a z. (z -> String -> a -> z) -> z -> Object a -> z
fold = _foldM ((#))

-- | Fold the keys and values of an object, accumulating values using some
-- | `Monoid`.
foldMap :: forall a m. Monoid m => (String -> a -> m) -> Object a -> m
foldMap f = fold (\acc k v -> acc <> f k v) mempty

-- | Fold the keys and values of an object, accumulating values and effects in
-- | some `Monad`.
foldM :: forall a m z. Monad m => (z -> String -> a -> m z) -> z -> Object a -> m z
foldM f z = _foldM bind f (pure z)

instance foldableObject :: Foldable Object where
  foldl f = fold (\z _ -> f z)
  foldr f z m = foldr f z (values m)
  foldMap f = foldMap (const f)

instance foldableWithIndexObject :: FoldableWithIndex String Object where
  foldlWithIndex f = fold (flip f)
  foldrWithIndex f z m = foldr (uncurry f) z (toArrayWithKey Tuple m)
  foldMapWithIndex = foldMap

instance traversableObject :: Traversable Object where
  traverse = traverseWithIndex <<< const
  sequence = traverse identity

instance traversableWithIndexObject :: TraversableWithIndex String Object where
  traverseWithIndex f ms =
    fold (\acc k v -> flip (insert k) <$> acc <*> f k v) (pure empty) ms

-- Unfortunately the above are not short-circuitable (consider using purescript-machines)
-- so we need special cases:

foreign import _foldSCObject :: forall a z. Fn4 (Object a) z (z -> String -> a -> Maybe z) (forall b. b -> Maybe b -> b) z

-- | Fold the keys and values of a map.
-- |
-- | This function allows the folding function to terminate the fold early,
-- | using `Maybe`.
foldMaybe :: forall a z. (z -> String -> a -> Maybe z) -> z -> Object a -> z
foldMaybe f z m = runFn4 _foldSCObject m z f fromMaybe

-- | Test whether all key/value pairs in a `Object` satisfy a predicate.
foreign import all :: forall a. (String -> a -> Boolean) -> Object a -> Boolean

instance eqObject :: Eq a => Eq (Object a) where
  eq m1 m2 = (isSubmap m1 m2) && (isSubmap m2 m1)

instance eq1Object :: Eq1 Object where
  eq1 = eq

-- Internal use
toAscArray :: forall v. Object v -> Array (Tuple String v)
toAscArray = toAscUnfoldable

instance ordObject :: Ord a => Ord (Object a) where
  compare m1 m2 = compare (toAscArray m1) (toAscArray m2)

instance showObject :: Show a => Show (Object a) where
  show m = "(fromFoldable " <> show (toArray m) <> ")"

-- | An empty map
foreign import empty :: forall a. Object a

-- | Test whether one map contains all of the keys and values contained in another map
isSubmap :: forall a. Eq a => Object a -> Object a -> Boolean
isSubmap m1 m2 = all f m1 where
  f k v = runFn4 _lookup false ((==) v) k m2

-- | Test whether a map is empty
isEmpty :: forall a. Object a -> Boolean
isEmpty = all (\_ _ -> false)

-- | Calculate the number of key/value pairs in a map
foreign import size :: forall a. Object a -> Int

-- | Create an `Object a` with one key/value pair
singleton :: forall a. String -> a -> Object a
singleton k v = runST (OST.poke k v =<< OST.new)

foreign import _lookup :: forall a z. Fn4 z (a -> z) String (Object a) z

-- | Lookup the value for a key in a map
lookup :: forall a. String -> Object a -> Maybe a
lookup = runFn4 _lookup Nothing Just

-- | Test whether a `String` appears as a key in a map
member :: forall a. String -> Object a -> Boolean
member = runFn4 _lookup false (const true)

-- | Insert or replace a key/value pair in a map
insert :: forall a. String -> a -> Object a -> Object a
insert k v = mutate (OST.poke k v)

-- | Delete a key and value from a map
delete :: forall a. String -> Object a -> Object a
delete k = mutate (OST.delete k)

-- | Delete a key and value from a map, returning the value
-- | as well as the subsequent map
pop :: forall a. String -> Object a -> Maybe (Tuple a (Object a))
pop k m = lookup k m <#> \a -> Tuple a (delete k m)

-- | Insert, remove or update a value for a key in a map
alter :: forall a. (Maybe a -> Maybe a) -> String -> Object a -> Object a
alter f k m = case f (k `lookup` m) of
  Nothing -> delete k m
  Just v -> insert k v m

-- | Remove or update a value for a key in a map
update :: forall a. (a -> Maybe a) -> String -> Object a -> Object a
update f k m = alter (maybe Nothing f) k m

-- | Create an `Object a` from a foldable collection of key/value pairs
fromFoldable :: forall f a. Foldable f => f (Tuple String a) -> Object a
fromFoldable l = runST do
  s <- OST.new
  ST.foreach (A.fromFoldable l) \(Tuple k v) -> void $ OST.poke k v s
  pure s

-- | Create an `Object a` from a `String`-indexed foldable collection
fromFoldableWithIndex :: forall f a. FoldableWithIndex String f => f a -> Object a
fromFoldableWithIndex l = runST do
  s <- OST.new
  forWithIndex_ l \k v -> OST.poke k v s
  pure s

foreign import _lookupST :: forall a r z. Fn4 z (a -> z) String (STObject r a) (ST r z)

-- | Create an `Object a` from a foldable collection of key/value pairs, using the
-- | specified function to combine values for duplicate keys.
fromFoldableWith :: forall f a. Foldable f => (a -> a -> a) -> f (Tuple String a) -> Object a
fromFoldableWith f l = runST (do
  s <- OST.new
  for_ l (\(Tuple k v) -> runFn4 _lookupST v (f v) k s >>= \v' -> OST.poke k v' s)
  pure s)

-- | Create an `Object a` from a homogeneous record, i.e. all of the record
-- | fields are of the same type.
fromHomogeneous :: forall r a. Homogeneous r a => { | r } -> Object a
fromHomogeneous = unsafeCoerce

foreign import toArrayWithKey :: forall a b . (String -> a -> b) -> Object a -> Array b

-- | Unfolds a map into a list of key/value pairs
toUnfoldable :: forall f a. Unfoldable f => Object a -> f (Tuple String a)
toUnfoldable = A.toUnfoldable <<< toArrayWithKey Tuple

-- | Unfolds a map into a list of key/value pairs which is guaranteed to be
-- | sorted by key
toAscUnfoldable :: forall f a. Unfoldable f => Object a -> f (Tuple String a)
toAscUnfoldable = A.toUnfoldable <<< A.sortWith fst <<< toArrayWithKey Tuple

-- Internal
toArray :: forall a. Object a -> Array (Tuple String a)
toArray = toArrayWithKey Tuple

-- | Get an array of the keys in a map
foreign import keys :: forall a. Object a -> Array String

-- | Get a list of the values in a map
values :: forall a. Object a -> Array a
values = toArrayWithKey (\_ v -> v)

-- | Compute the union of two maps, preferring the first map in the case of
-- | duplicate keys.
union :: forall a. Object a -> Object a -> Object a
union m = mutate (\s -> foldM (\s' k v -> OST.poke k v s') s m)

-- | Compute the union of two maps, using the specified function
-- | to combine values for duplicate keys.
unionWith :: forall a. (a -> a -> a) -> Object a -> Object a -> Object a
unionWith f m1 m2 =
  mutate (\s1 -> foldM (\s2 k v1 -> OST.poke k (runFn4 _lookup v1 (\v2 -> f v1 v2) k m2) s2) s1 m1) m2

-- | Compute the union of a collection of maps
unions :: forall f a. Foldable f => f (Object a) -> Object a
unions = foldl union empty

foreign import _mapWithKey :: forall a b. Fn2 (Object a) (String -> a -> b) (Object b)

-- | Apply a function of two arguments to each key/value pair, producing a new map
mapWithKey :: forall a b. (String -> a -> b) -> Object a -> Object b
mapWithKey f m = runFn2 _mapWithKey m f

instance semigroupObject :: (Semigroup a) => Semigroup (Object a) where
  append = unionWith (<>)

instance monoidObject :: (Semigroup a) => Monoid (Object a) where
  mempty = empty

-- | Filter out those key/value pairs of a map for which a predicate
-- | fails to hold.
filterWithKey :: forall a. (String -> a -> Boolean) -> Object a -> Object a
filterWithKey predicate m = runST go
  where
  go :: forall r. ST r (STObject r a)
  go = do
    m' <- OST.new
    foldM step m' m
    where
      step acc k v = if predicate k v then OST.poke k v acc else pure acc

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the key fails to hold.
filterKeys :: (String -> Boolean) -> Object ~> Object
filterKeys predicate = filterWithKey $ const <<< predicate

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the value fails to hold.
filter :: forall a. (a -> Boolean) -> Object a -> Object a
filter predicate = filterWithKey $ const predicate
