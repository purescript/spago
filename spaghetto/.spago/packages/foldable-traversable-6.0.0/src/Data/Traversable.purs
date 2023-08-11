module Data.Traversable
  ( class Traversable, traverse, sequence
  , traverseDefault, sequenceDefault
  , for
  , scanl
  , scanr
  , mapAccumL
  , mapAccumR
  , module Data.Foldable
  , module Data.Traversable.Accum
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, all, and, any, elem, find, fold, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for_, intercalate, maximum, maximumBy, minimum, minimumBy, notElem, oneOf, or, sequence_, sum, traverse_)
import Data.Functor.App (App(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct (Coproduct(..), coproduct)
import Data.Functor.Product (Product(..), product)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Maybe.Last (Last(..))
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Traversable.Accum (Accum)
import Data.Traversable.Accum.Internal (StateL(..), StateR(..), stateL, stateR)
import Data.Tuple (Tuple(..))

-- | `Traversable` represents data structures which can be _traversed_,
-- | accumulating results and effects in some `Applicative` functor.
-- |
-- | - `traverse` runs an action for every element in a data structure,
-- |   and accumulates the results.
-- | - `sequence` runs the actions _contained_ in a data structure,
-- |   and accumulates the results.
-- |
-- | ```purescript
-- | import Data.Traversable
-- | import Data.Maybe
-- | import Data.Int (fromNumber)
-- |
-- | sequence [Just 1, Just 2, Just 3] == Just [1,2,3]
-- | sequence [Nothing, Just 2, Just 3] == Nothing
-- |
-- | traverse fromNumber [1.0, 2.0, 3.0] == Just [1,2,3]
-- | traverse fromNumber [1.5, 2.0, 3.0] == Nothing
-- |
-- | traverse logShow [1,2,3]
-- | -- prints:
-- |    1
-- |    2
-- |    3
-- |
-- | traverse (\x -> [x, 0]) [1,2,3] == [[1,2,3],[1,2,0],[1,0,3],[1,0,0],[0,2,3],[0,2,0],[0,0,3],[0,0,0]]
-- | ```
-- |
-- | The `traverse` and `sequence` functions should be compatible in the
-- | following sense:
-- |
-- | - `traverse f xs = sequence (f <$> xs)`
-- | - `sequence = traverse identity`
-- |
-- | `Traversable` instances should also be compatible with the corresponding
-- | `Foldable` instances, in the following sense:
-- |
-- | - `foldMap f = runConst <<< traverse (Const <<< f)`
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `traverseDefault`
-- | - `sequenceDefault`
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  sequence :: forall a m. Applicative m => t (m a) -> m (t a)

-- | A default implementation of `traverse` using `sequence` and `map`.
traverseDefault
  :: forall t a b m
   . Traversable t
  => Applicative m
  => (a -> m b)
  -> t a
  -> m (t b)
traverseDefault f ta = sequence (f <$> ta)

-- | A default implementation of `sequence` using `traverse`.
sequenceDefault
  :: forall t a m
   . Traversable t
  => Applicative m
  => t (m a)
  -> m (t a)
sequenceDefault = traverse identity

instance traversableArray :: Traversable Array where
  traverse = traverseArrayImpl apply map pure
  sequence = sequenceDefault

foreign import traverseArrayImpl
  :: forall m a b
   . (forall x y. m (x -> y) -> m x -> m y)
  -> (forall x y. (x -> y) -> m x -> m y)
  -> (forall x. x -> m x)
  -> (a -> m b)
  -> Array a
  -> m (Array b)

instance traversableMaybe :: Traversable Maybe where
  traverse _ Nothing  = pure Nothing
  traverse f (Just x) = Just <$> f x
  sequence Nothing  = pure Nothing
  sequence (Just x) = Just <$> x

instance traversableFirst :: Traversable First where
  traverse f (First x) = First <$> traverse f x
  sequence (First x) = First <$> sequence x

instance traversableLast :: Traversable Last where
  traverse f (Last x) = Last <$> traverse f x
  sequence (Last x) = Last <$> sequence x

instance traversableAdditive :: Traversable Additive where
  traverse f (Additive x) = Additive <$> f x
  sequence (Additive x) = Additive <$> x

instance traversableDual :: Traversable Dual where
  traverse f (Dual x) = Dual <$> f x
  sequence (Dual x) = Dual <$> x

instance traversableConj :: Traversable Conj where
  traverse f (Conj x) = Conj <$> f x
  sequence (Conj x) = Conj <$> x

instance traversableDisj :: Traversable Disj where
  traverse f (Disj x) = Disj <$> f x
  sequence (Disj x) = Disj <$> x

instance traversableMultiplicative :: Traversable Multiplicative where
  traverse f (Multiplicative x) = Multiplicative <$> f x
  sequence (Multiplicative x) = Multiplicative <$> x

instance traversableEither :: Traversable (Either a) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right x) = Right <$> f x
  sequence (Left x) = pure (Left x)
  sequence (Right x)  = Right <$> x

instance traversableTuple :: Traversable (Tuple a) where
  traverse f (Tuple x y) = Tuple x <$> f y
  sequence (Tuple x y) = Tuple x <$> y

instance traversableIdentity :: Traversable Identity where
  traverse f (Identity x) = Identity <$> f x
  sequence (Identity x) = Identity <$> x

instance traversableConst :: Traversable (Const a) where
  traverse _ (Const x) = pure (Const x)
  sequence (Const x) = pure (Const x)

instance traversableProduct :: (Traversable f, Traversable g) => Traversable (Product f g) where
  traverse f (Product (Tuple fa ga)) = lift2 product (traverse f fa) (traverse f ga)
  sequence (Product (Tuple fa ga)) = lift2 product (sequence fa) (sequence ga)

instance traversableCoproduct :: (Traversable f, Traversable g) => Traversable (Coproduct f g) where
  traverse f = coproduct
    (map (Coproduct <<< Left) <<< traverse f)
    (map (Coproduct <<< Right) <<< traverse f)
  sequence = coproduct
    (map (Coproduct <<< Left) <<< sequence)
    (map (Coproduct <<< Right) <<< sequence)

instance traversableCompose :: (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = map Compose $ traverse (traverse f) fga
  sequence = traverse identity

instance traversableApp :: Traversable f => Traversable (App f) where
  traverse f (App x) = App <$> traverse f x
  sequence (App x) = App <$> sequence x

-- | A version of `traverse` with its arguments flipped.
-- |
-- |
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure:
-- |
-- | For example:
-- |
-- | ```purescript
-- | for [1, 2, 3] \n -> do
-- |   print n
-- |   return (n * n)
-- | ```
for
  :: forall a b m t
   . Applicative m
  => Traversable t
  => t a
  -> (a -> m b)
  -> m (t b)
for x f = traverse f x

-- | Fold a data structure from the left, keeping all intermediate results
-- | instead of only the final result. Note that the initial value does not
-- | appear in the result (unlike Haskell's `Prelude.scanl`).
-- |
-- | ```purescript
-- | scanl (+) 0  [1,2,3] = [1,3,6]
-- | scanl (-) 10 [1,2,3] = [9,7,4]
-- | ```
scanl :: forall a b f. Traversable f => (b -> a -> b) -> b -> f a -> f b
scanl f b0 xs = (mapAccumL (\b a -> let b' = f b a in { accum: b', value: b' }) b0 xs).value

-- | Fold a data structure from the left, keeping all intermediate results
-- | instead of only the final result.
-- |
-- | Unlike `scanl`, `mapAccumL` allows the type of accumulator to differ
-- | from the element type of the final data structure.
mapAccumL
  :: forall a b s f
   . Traversable f
  => (s -> a -> Accum s b)
  -> s
  -> f a
  -> Accum s (f b)
mapAccumL f s0 xs = stateL (traverse (\a -> StateL \s -> f s a) xs) s0

-- | Fold a data structure from the right, keeping all intermediate results
-- | instead of only the final result. Note that the initial value does not
-- | appear in the result (unlike Haskell's `Prelude.scanr`).
-- |
-- | ```purescript
-- | scanr (+) 0 [1,2,3] = [6,5,3]
-- | scanr (flip (-)) 10 [1,2,3] = [4,5,7]
-- | ```
scanr :: forall a b f. Traversable f => (a -> b -> b) -> b -> f a -> f b
scanr f b0 xs = (mapAccumR (\b a -> let b' = f a b in { accum: b', value: b' }) b0 xs).value

-- | Fold a data structure from the right, keeping all intermediate results
-- | instead of only the final result.
-- |
-- | Unlike `scanr`, `mapAccumR` allows the type of accumulator to differ
-- | from the element type of the final data structure.
mapAccumR
  :: forall a b s f
   . Traversable f
  => (s -> a -> Accum s b)
  -> s
  -> f a
  -> Accum s (f b)
mapAccumR f s0 xs = stateR (traverse (\a -> StateR \s -> f s a) xs) s0
