module Data.Foldable
  ( class Foldable, foldr, foldl, foldMap
  , foldrDefault, foldlDefault, foldMapDefaultL, foldMapDefaultR
  , fold
  , foldM
  , traverse_
  , for_
  , sequence_
  , oneOf
  , oneOfMap
  , intercalate
  , surroundMap
  , surround
  , and
  , or
  , all
  , any
  , sum
  , product
  , elem
  , notElem
  , indexl
  , indexr
  , find
  , findMap
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , null
  , length
  , lookup
  ) where

import Prelude

import Control.Plus (class Plus, alt, empty)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Functor.App (App(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.Functor.Product (Product(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Maybe.Last (Last(..))
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (alaF, unwrap)
import Data.Tuple (Tuple(..))

-- | `Foldable` represents data structures which can be _folded_.
-- |
-- | - `foldr` folds a structure from the right
-- | - `foldl` folds a structure from the left
-- | - `foldMap` folds a structure by accumulating values in a `Monoid`
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `foldrDefault`
-- | - `foldlDefault`
-- | - `foldMapDefaultR`
-- | - `foldMapDefaultL`
-- |
-- | Note: some combinations of the default implementations are unsafe to
-- | use together - causing a non-terminating mutually recursive cycle.
-- | These combinations are documented per function.
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m

-- | A default implementation of `foldr` using `foldMap`.
-- |
-- | Note: when defining a `Foldable` instance, this function is unsafe to use
-- | in combination with `foldMapDefaultR`.
foldrDefault
  :: forall f a b
   . Foldable f
  => (a -> b -> b)
  -> b
  -> f a
  -> b
foldrDefault c u xs = unwrap (foldMap (Endo <<< c) xs) u

-- | A default implementation of `foldl` using `foldMap`.
-- |
-- | Note: when defining a `Foldable` instance, this function is unsafe to use
-- | in combination with `foldMapDefaultL`.
foldlDefault
  :: forall f a b
   . Foldable f
  => (b -> a -> b)
  -> b
  -> f a
  -> b
foldlDefault c u xs = unwrap (unwrap (foldMap (Dual <<< Endo <<< flip c) xs)) u

-- | A default implementation of `foldMap` using `foldr`.
-- |
-- | Note: when defining a `Foldable` instance, this function is unsafe to use
-- | in combination with `foldrDefault`.
foldMapDefaultR
  :: forall f a m
   . Foldable f
  => Monoid m
  => (a -> m)
  -> f a
  -> m
foldMapDefaultR f = foldr (\x acc -> f x <> acc) mempty

-- | A default implementation of `foldMap` using `foldl`.
-- |
-- | Note: when defining a `Foldable` instance, this function is unsafe to use
-- | in combination with `foldlDefault`.
foldMapDefaultL
  :: forall f a m
   . Foldable f
  => Monoid m
  => (a -> m)
  -> f a
  -> m
foldMapDefaultL f = foldl (\acc x -> acc <> f x) mempty

instance foldableArray :: Foldable Array where
  foldr = foldrArray
  foldl = foldlArray
  foldMap = foldMapDefaultR

foreign import foldrArray :: forall a b. (a -> b -> b) -> b -> Array a -> b
foreign import foldlArray :: forall a b. (b -> a -> b) -> b -> Array a -> b

instance foldableMaybe :: Foldable Maybe where
  foldr _ z Nothing  = z
  foldr f z (Just x) = x `f` z
  foldl _ z Nothing  = z
  foldl f z (Just x) = z `f` x
  foldMap _ Nothing  = mempty
  foldMap f (Just x) = f x

instance foldableFirst :: Foldable First where
  foldr f z (First x) = foldr f z x
  foldl f z (First x) = foldl f z x
  foldMap f (First x) = foldMap f x

instance foldableLast :: Foldable Last where
  foldr f z (Last x) = foldr f z x
  foldl f z (Last x) = foldl f z x
  foldMap f (Last x) = foldMap f x

instance foldableAdditive :: Foldable Additive where
  foldr f z (Additive x) = x `f` z
  foldl f z (Additive x) = z `f` x
  foldMap f (Additive x) = f x

instance foldableDual :: Foldable Dual where
  foldr f z (Dual x) = x `f` z
  foldl f z (Dual x) = z `f` x
  foldMap f (Dual x) = f x

instance foldableDisj :: Foldable Disj where
  foldr f z (Disj x) = f x z
  foldl f z (Disj x) = f z x
  foldMap f (Disj x) = f x

instance foldableConj :: Foldable Conj where
  foldr f z (Conj x) = f x z
  foldl f z (Conj x) = f z x
  foldMap f (Conj x) = f x

instance foldableMultiplicative :: Foldable Multiplicative where
  foldr f z (Multiplicative x) = x `f` z
  foldl f z (Multiplicative x) = z `f` x
  foldMap f (Multiplicative x) = f x

instance foldableEither :: Foldable (Either a) where
  foldr _ z (Left _)  = z
  foldr f z (Right x) = f x z
  foldl _ z (Left _)  = z
  foldl f z (Right x) = f z x
  foldMap _ (Left _)  = mempty
  foldMap f (Right x) = f x

instance foldableTuple :: Foldable (Tuple a) where
  foldr f z (Tuple _ x) = f x z
  foldl f z (Tuple _ x) = f z x
  foldMap f (Tuple _ x) = f x

instance foldableIdentity :: Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

instance foldableConst :: Foldable (Const a) where
  foldr _ z _ = z
  foldl _ z _ = z
  foldMap _ _ = mempty

instance foldableProduct :: (Foldable f, Foldable g) => Foldable (Product f g) where
  foldr f z (Product (Tuple fa ga)) = foldr f (foldr f z ga) fa
  foldl f z (Product (Tuple fa ga)) = foldl f (foldl f z fa) ga
  foldMap f (Product (Tuple fa ga)) = foldMap f fa <> foldMap f ga

instance foldableCoproduct :: (Foldable f, Foldable g) => Foldable (Coproduct f g) where
  foldr f z = coproduct (foldr f z) (foldr f z)
  foldl f z = coproduct (foldl f z) (foldl f z)
  foldMap f = coproduct (foldMap f) (foldMap f)

instance foldableCompose :: (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldr f i (Compose fga) = foldr (flip (foldr f)) i fga
  foldl f i (Compose fga) = foldl (foldl f) i fga
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance foldableApp :: Foldable f => Foldable (App f) where
  foldr f i (App x) = foldr f i x
  foldl f i (App x) = foldl f i x
  foldMap f (App x) = foldMap f x

-- | Fold a data structure, accumulating values in some `Monoid`.
fold :: forall f m. Foldable f => Monoid m => f m -> m
fold = foldMap identity

-- | Similar to 'foldl', but the result is encapsulated in a monad.
-- |
-- | Note: this function is not generally stack-safe, e.g., for monads which
-- | build up thunks a la `Eff`.
foldM :: forall f m a b. Foldable f => Monad m => (b -> a -> m b) -> b -> f a -> m b
foldM f b0 = foldl (\b a -> b >>= flip f a) (pure b0)

-- | Traverse a data structure, performing some effects encoded by an
-- | `Applicative` functor at each value, ignoring the final result.
-- |
-- | For example:
-- |
-- | ```purescript
-- | traverse_ print [1, 2, 3]
-- | ```
traverse_
  :: forall a b f m
   . Applicative m
  => Foldable f
  => (a -> m b)
  -> f a
  -> m Unit
traverse_ f = foldr ((*>) <<< f) (pure unit)

-- | A version of `traverse_` with its arguments flipped.
-- |
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure:
-- |
-- | For example:
-- |
-- | ```purescript
-- | for_ [1, 2, 3] \n -> do
-- |   print n
-- |   trace "squared is"
-- |   print (n * n)
-- | ```
for_
  :: forall a b f m
   . Applicative m
  => Foldable f
  => f a
  -> (a -> m b)
  -> m Unit
for_ = flip traverse_

-- | Perform all of the effects in some data structure in the order
-- | given by the `Foldable` instance, ignoring the final result.
-- |
-- | For example:
-- |
-- | ```purescript
-- | sequence_ [ trace "Hello, ", trace " world!" ]
-- | ```
sequence_ :: forall a f m. Applicative m => Foldable f => f (m a) -> m Unit
sequence_ = traverse_ identity

-- | Combines a collection of elements using the `Alt` operation.
oneOf :: forall f g a. Foldable f => Plus g => f (g a) -> g a
oneOf = foldr alt empty

-- | Folds a structure into some `Plus`.
oneOfMap :: forall f g a b. Foldable f => Plus g => (a -> g b) -> f a -> g b
oneOfMap f = foldr (alt <<< f) empty

-- | Fold a data structure, accumulating values in some `Monoid`,
-- | combining adjacent elements using the specified separator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | > intercalate ", " ["Lorem", "ipsum", "dolor"]
-- | = "Lorem, ipsum, dolor"
-- |
-- | > intercalate "*" ["a", "b", "c"]
-- | = "a*b*c"
-- |
-- | > intercalate [1] [[2, 3], [4, 5], [6, 7]]
-- | = [2, 3, 1, 4, 5, 1, 6, 7]
-- | ```
intercalate :: forall f m. Foldable f => Monoid m => m -> f m -> m
intercalate sep xs = (foldl go { init: true, acc: mempty } xs).acc
  where
  go { init: true } x = { init: false, acc: x }
  go { acc: acc }   x = { init: false, acc: acc <> sep <> x }

-- | `foldMap` but with each element surrounded by some fixed value.
-- |
-- | For example:
-- |
-- | ```purescript
-- | > surroundMap "*" show []
-- | = "*"
-- |
-- | > surroundMap "*" show [1]
-- | = "*1*"
-- |
-- | > surroundMap "*" show [1, 2]
-- | = "*1*2*"
-- |
-- | > surroundMap "*" show [1, 2, 3]
-- | = "*1*2*3*"
-- | ```
surroundMap :: forall f a m. Foldable f => Semigroup m => m -> (a -> m) -> f a -> m
surroundMap d t f = unwrap (foldMap joined f) d
  where joined a = Endo \m -> d <> t a <> m

-- | `fold` but with each element surrounded by some fixed value.
-- |
-- | For example:
-- |
-- | ```purescript
-- | > surround "*" []
-- | = "*"
-- |
-- | > surround "*" ["1"]
-- | = "*1*"
-- |
-- | > surround "*" ["1", "2"]
-- | = "*1*2*"
-- |
-- | > surround "*" ["1", "2", "3"]
-- | = "*1*2*3*"
-- | ```
surround :: forall f m. Foldable f => Semigroup m => m -> f m -> m
surround d = surroundMap d identity

-- | The conjunction of all the values in a data structure. When specialized
-- | to `Boolean`, this function will test whether all of the values in a data
-- | structure are `true`.
and :: forall a f. Foldable f => HeytingAlgebra a => f a -> a
and = all identity

-- | The disjunction of all the values in a data structure. When specialized
-- | to `Boolean`, this function will test whether any of the values in a data
-- | structure is `true`.
or :: forall a f. Foldable f => HeytingAlgebra a => f a -> a
or = any identity

-- | `all f` is the same as `and <<< map f`; map a function over the structure,
-- | and then get the conjunction of the results.
all :: forall a b f. Foldable f => HeytingAlgebra b => (a -> b) -> f a -> b
all  = alaF Conj foldMap

-- | `any f` is the same as `or <<< map f`; map a function over the structure,
-- | and then get the disjunction of the results.
any :: forall a b f. Foldable f => HeytingAlgebra b => (a -> b) -> f a -> b
any = alaF Disj foldMap

-- | Find the sum of the numeric values in a data structure.
sum :: forall a f. Foldable f => Semiring a => f a -> a
sum = foldl (+) zero

-- | Find the product of the numeric values in a data structure.
product :: forall a f. Foldable f => Semiring a => f a -> a
product = foldl (*) one

-- | Test whether a value is an element of a data structure.
elem :: forall a f. Foldable f => Eq a => a -> f a -> Boolean
elem = any <<< (==)

-- | Test whether a value is not an element of a data structure.
notElem :: forall a f. Foldable f => Eq a => a -> f a -> Boolean
notElem x = not <<< elem x

-- | Try to get nth element from the left in a data structure
indexl :: forall a f. Foldable f => Int -> f a -> Maybe a
indexl idx = _.elem <<< foldl go { elem: Nothing, pos: 0 }
  where
  go cursor a =
    case cursor.elem of
      Just _ -> cursor
      _ ->
        if cursor.pos == idx
          then { elem: Just a, pos: cursor.pos }
          else { pos: cursor.pos + 1, elem: cursor.elem }

-- | Try to get nth element from the right in a data structure
indexr :: forall a f. Foldable f => Int -> f a -> Maybe a
indexr idx = _.elem <<< foldr go { elem: Nothing, pos: 0 }
  where
  go a cursor =
    case cursor.elem of
      Just _ -> cursor
      _ ->
        if cursor.pos == idx
          then { elem: Just a, pos: cursor.pos }
          else { pos: cursor.pos + 1, elem: cursor.elem }

-- | Try to find an element in a data structure which satisfies a predicate.
find :: forall a f. Foldable f => (a -> Boolean) -> f a -> Maybe a
find p = foldl go Nothing
  where
  go Nothing x | p x = Just x
  go r _ = r

-- | Try to find an element in a data structure which satisfies a predicate mapping.
findMap :: forall a b f. Foldable f => (a -> Maybe b) -> f a -> Maybe b
findMap p = foldl go Nothing
  where
  go Nothing x = p x
  go r _ = r

-- | Find the largest element of a structure, according to its `Ord` instance.
maximum :: forall a f. Ord a => Foldable f => f a -> Maybe a
maximum = maximumBy compare

-- | Find the largest element of a structure, according to a given comparison
-- | function. The comparison function should represent a total ordering (see
-- | the `Ord` type class laws); if it does not, the behaviour is undefined.
maximumBy :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
maximumBy cmp = foldl max' Nothing
  where
  max' Nothing x  = Just x
  max' (Just x) y = Just (if cmp x y == GT then x else y)

-- | Find the smallest element of a structure, according to its `Ord` instance.
minimum :: forall a f. Ord a => Foldable f => f a -> Maybe a
minimum = minimumBy compare

-- | Find the smallest element of a structure, according to a given comparison
-- | function. The comparison function should represent a total ordering (see
-- | the `Ord` type class laws); if it does not, the behaviour is undefined.
minimumBy :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy cmp = foldl min' Nothing
  where
  min' Nothing x  = Just x
  min' (Just x) y = Just (if cmp x y == LT then x else y)

-- | Test whether the structure is empty.
-- | Optimized for structures that are similar to cons-lists, because there
-- | is no general way to do better.
null :: forall a f. Foldable f => f a -> Boolean
null = foldr (\_ _ -> false) true

-- | Returns the size/length of a finite structure.
-- | Optimized for structures that are similar to cons-lists, because there
-- | is no general way to do better.
length :: forall a b f. Foldable f => Semiring b => f a -> b
length = foldl (\c _ -> add one c) zero

-- | Lookup a value in a data structure of `Tuple`s, generalizing association lists.
lookup :: forall a b f. Foldable f => Eq a => a -> f (Tuple a b) -> Maybe b
lookup a = unwrap <<< foldMap \(Tuple a' b) -> First (if a == a' then Just b else Nothing)
