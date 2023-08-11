module Control.Monad.Gen
  ( module Control.Monad.Gen.Class
  , choose
  , oneOf
  , frequency
  , elements
  , unfoldable
  , suchThat
  , filtered
  ) where

import Prelude

import Control.Monad.Gen.Class (class MonadGen, Size, chooseBool, chooseFloat, chooseInt, resize, sized)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Foldable (foldMap, foldr, length)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF, un)
import Data.Semigroup.Foldable (class Foldable1, foldMap1)
import Data.Semigroup.Last (Last(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable, unfoldr)

data LL a = Cons a (LL a) | Nil

-- | Creates a generator that outputs a value chosen from one of two existing
-- | existing generators with even probability.
choose :: forall m a. MonadGen m => m a -> m a -> m a
choose genA genB = chooseBool >>= if _ then genA else genB

-- | Creates a generator that outputs a value chosen from a selection of
-- | existing generators with uniform probability.
oneOf :: forall m f a. MonadGen m => Foldable1 f => f (m a) -> m a
oneOf xs = do
  n <- chooseInt 0 (length xs - 1)
  fromIndex n xs

newtype FreqSemigroup a = FreqSemigroup (Number -> Tuple (Maybe Number) a)

freqSemigroup :: forall a. Tuple Number a -> FreqSemigroup a
freqSemigroup (Tuple weight x) =
  FreqSemigroup \pos ->
    if pos >= weight
      then Tuple (Just (pos - weight)) x
      else Tuple Nothing x

getFreqVal :: forall a. FreqSemigroup a -> Number -> a
getFreqVal (FreqSemigroup f) = snd <<< f

instance semigroupFreqSemigroup :: Semigroup (FreqSemigroup a) where
  append (FreqSemigroup f) (FreqSemigroup g) =
    FreqSemigroup \pos ->
      case f pos of
        Tuple (Just pos') _ -> g pos'
        result -> result

-- | Creates a generator that outputs a value chosen from a selection of
-- | existing generators, where the selection has weight values for the
-- | probability of choice for each generator. The probability values will be
-- | normalised.
frequency
  :: forall m f a
   . MonadGen m
  => Foldable1 f
  => f (Tuple Number (m a))
  -> m a
frequency xs =
  let total = alaF Additive foldMap fst xs
  in  chooseFloat 0.0 total >>= getFreqVal (foldMap1 freqSemigroup xs)

-- | Creates a generator that outputs a value chosen from a selection with
-- | uniform probability.
elements :: forall m f a. MonadGen m => Foldable1 f => f a -> m a
elements xs = do
  n <- chooseInt 0 (length xs - 1)
  pure $ fromIndex n xs

-- | Creates a generator that produces unfoldable structures based on an
-- | existing generator for the elements.
-- |
-- | The size of the unfoldable will be determined by the current size state
-- | for the generator. To generate an unfoldable structure of a particular
-- | size, use the `resize` function from the `MonadGen` class first.
unfoldable
  :: forall m f a
   . MonadRec m
  => MonadGen m
  => Unfoldable f
  => m a
  -> m (f a)
unfoldable gen = unfoldr unfold <$> sized (tailRecM loopGen <<< Tuple Nil)
  where
  loopGen :: Tuple (LL a) Int -> m (Step (Tuple (LL a) Int) (LL a))
  loopGen (Tuple acc n)
    | n <= 0 =
        pure $ Done acc
    | otherwise = do
        x <- gen
        pure $ Loop (Tuple (Cons x acc) (n - 1))
  unfold :: LL a -> Maybe (Tuple a (LL a))
  unfold = case _ of
    Nil -> Nothing
    Cons x xs -> Just (Tuple x xs)

-- | Creates a generator that repeatedly run another generator until its output
-- | matches a given predicate. This will never halt if the predicate always
-- | fails.
suchThat :: forall m a. MonadRec m => MonadGen m => m a -> (a -> Boolean) -> m a
suchThat gen pred = filtered $ gen <#> \a -> if pred a then Just a else Nothing

-- | Creates a generator that repeatedly run another generator until it produces
-- | `Just` node. This will never halt if the input generator always produces `Nothing`.
filtered :: forall m a. MonadRec m => MonadGen m => m (Maybe a) -> m a
filtered gen = tailRecM go unit
  where
  go :: Unit -> m (Step Unit a)
  go _ = gen <#> \a -> case a of
    Nothing -> Loop unit
    Just a' -> Done a'

-- | Internal: get the Foldable element at index i.
-- | If the index is <= 0, return the first element.
-- | If it's >= length, return the last.
fromIndex :: forall f a. Foldable1 f => Int -> f a -> a
fromIndex i xs = go i (foldr Cons Nil xs)
  where
    go _ (Cons a Nil) = a
    go j (Cons a _) | j <= 0 = a
    go j (Cons _ as) = go (j - 1) as
    -- next case is "impossible", but serves as proof of non-emptyness
    go _ Nil = un Last (foldMap1 Last xs) 
