module Data.Semigroup.Foldable
  ( class Foldable1
  , foldMap1
  , fold1
  , foldr1
  , foldl1
  , traverse1_
  , for1_
  , sequence1_
  , foldr1Default
  , foldl1Default
  , foldMap1DefaultR
  , foldMap1DefaultL
  , intercalate
  , intercalateMap
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Identity (Identity(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (ala, alaF)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Tuple (Tuple(..))

-- | `Foldable1` represents data structures with a minimum of one element that can be _folded_.
-- |
-- | - `foldr1` folds a structure from the right
-- | - `foldl1` folds a structure from the left
-- | - `foldMap1` folds a structure by accumulating values in a `Semigroup`
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `foldr1Default`
-- | - `foldl1Default`
-- | - `foldMap1DefaultR`
-- | - `foldMap1DefaultL`
-- |
-- | Note: some combinations of the default implementations are unsafe to
-- | use together - causing a non-terminating mutually recursive cycle.
-- | These combinations are documented per function.
class Foldable t <= Foldable1 t where
  foldr1 :: forall a. (a -> a -> a) -> t a -> a
  foldl1 :: forall a. (a -> a -> a) -> t a -> a
  foldMap1 :: forall a m. Semigroup m => (a -> m) -> t a -> m

-- | A default implementation of `foldr1` using `foldMap1`.
-- |
-- | Note: when defining a `Foldable1` instance, this function is unsafe to use
-- | in combination with `foldMap1DefaultR`.
foldr1Default :: forall t a. Foldable1 t => (a -> a -> a) -> t a -> a
foldr1Default = flip (runFoldRight1 <<< foldMap1 mkFoldRight1)

-- | A default implementation of `foldl1` using `foldMap1`.
-- |
-- | Note: when defining a `Foldable1` instance, this function is unsafe to use
-- | in combination with `foldMap1DefaultL`.
foldl1Default :: forall t a. Foldable1 t => (a -> a -> a) -> t a -> a
foldl1Default = flip (runFoldRight1 <<< alaF Dual foldMap1 mkFoldRight1) <<< flip

-- | A default implementation of `foldMap1` using `foldr1`.
-- |
-- | Note: when defining a `Foldable1` instance, this function is unsafe to use
-- | in combination with `foldr1Default`.
foldMap1DefaultR :: forall t m a. Foldable1 t => Functor t => Semigroup m => (a -> m) -> t a -> m
foldMap1DefaultR f = map f >>> foldr1 (<>)

-- | A default implementation of `foldMap1` using `foldl1`.
-- |
-- | Note: when defining a `Foldable1` instance, this function is unsafe to use
-- | in combination with `foldl1Default`.
foldMap1DefaultL :: forall t m a. Foldable1 t => Functor t => Semigroup m => (a -> m) -> t a -> m
foldMap1DefaultL f = map f >>> foldl1 (<>)

instance foldableDual :: Foldable1 Dual where
  foldr1 _ (Dual x) = x
  foldl1 _ (Dual x) = x
  foldMap1 f (Dual x) = f x

instance foldableMultiplicative :: Foldable1 Multiplicative where
  foldr1 _ (Multiplicative x) = x
  foldl1 _ (Multiplicative x) = x
  foldMap1 f (Multiplicative x) = f x

instance foldableTuple :: Foldable1 (Tuple a) where
  foldMap1 f (Tuple _ x) = f x
  foldr1 _ (Tuple _ x) = x
  foldl1 _ (Tuple _ x) = x

instance foldableIdentity :: Foldable1 Identity where
  foldMap1 f (Identity x) = f x
  foldl1 _ (Identity x) = x
  foldr1 _ (Identity x) = x

-- | Fold a data structure, accumulating values in some `Semigroup`.
fold1 :: forall t m. Foldable1 t => Semigroup m => t m -> m
fold1 = foldMap1 identity

newtype Act :: forall k. (k -> Type) -> k -> Type
newtype Act f a = Act (f a)

getAct :: forall f a. Act f a -> f a
getAct (Act f) = f

instance semigroupAct :: Apply f => Semigroup (Act f a) where
  append (Act a) (Act b) = Act (a *> b)

-- | Traverse a data structure, performing some effects encoded by an
-- | `Apply` instance at each value, ignoring the final result.
traverse1_ :: forall t f a b. Foldable1 t => Apply f => (a -> f b) -> t a -> f Unit
traverse1_ f t = unit <$ getAct (foldMap1 (Act <<< f) t)

-- | A version of `traverse1_` with its arguments flipped.
-- |
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure:
for1_ :: forall t f a b. Foldable1 t => Apply f => t a -> (a -> f b) -> f Unit
for1_ = flip traverse1_

-- | Perform all of the effects in some data structure in the order
-- | given by the `Foldable1` instance, ignoring the final result.
sequence1_ :: forall t f a. Foldable1 t => Apply f => t (f a) -> f Unit
sequence1_ = traverse1_ identity

maximum :: forall f a. Ord a => Foldable1 f => f a -> a
maximum = ala Max foldMap1

maximumBy :: forall f a. Foldable1 f => (a -> a -> Ordering) -> f a -> a
maximumBy cmp = foldl1 \x y -> if cmp x y == GT then x else y

minimum :: forall f a. Ord a => Foldable1 f => f a -> a
minimum = ala Min foldMap1

minimumBy :: forall f a. Foldable1 f => (a -> a -> Ordering) -> f a -> a
minimumBy cmp = foldl1 \x y -> if cmp x y == LT then x else y

-- | Internal. Used by intercalation functions.
newtype JoinWith a = JoinWith (a -> a)

joinee :: forall a. JoinWith a -> a -> a
joinee (JoinWith x) = x

instance semigroupJoinWith :: Semigroup a => Semigroup (JoinWith a) where
  append (JoinWith a) (JoinWith b) = JoinWith $ \j -> a j <> j <> b j

-- | Fold a data structure using a `Semigroup` instance,
-- | combining adjacent elements using the specified separator.
intercalate :: forall f m. Foldable1 f => Semigroup m => m -> f m -> m
intercalate = flip intercalateMap identity

-- | Fold a data structure, accumulating values in some `Semigroup`,
-- | combining adjacent elements using the specified separator.
intercalateMap
  :: forall f m a
   . Foldable1 f
  => Semigroup m
  => m -> (a -> m) -> f a -> m
intercalateMap j f foldable =
  joinee (foldMap1 (JoinWith <<< const <<< f) foldable) j

-- | Internal. Used by foldr1Default and foldl1Default.
data FoldRight1 a = FoldRight1 (a -> (a -> a -> a) -> a) a

instance foldRight1Semigroup :: Semigroup (FoldRight1 a) where
  append (FoldRight1 lf lr) (FoldRight1 rf rr) = FoldRight1 (\a f -> lf (f lr (rf a f)) f) rr

mkFoldRight1 :: forall a. a -> FoldRight1 a
mkFoldRight1 = FoldRight1 const

runFoldRight1 :: forall a. FoldRight1 a -> (a -> a -> a) -> a
runFoldRight1 (FoldRight1 f a) = f a
