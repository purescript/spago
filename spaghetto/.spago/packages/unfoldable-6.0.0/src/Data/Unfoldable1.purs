module Data.Unfoldable1
  ( class Unfoldable1, unfoldr1
  , replicate1
  , replicate1A
  , singleton
  , range
  , iterateN
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Semigroup.Traversable (class Traversable1, sequence1)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

-- | This class identifies data structures which can be _unfolded_.
-- |
-- | The generating function `f` in `unfoldr1 f` corresponds to the `uncons`
-- | operation of a non-empty list or array; it always returns a value, and
-- | then optionally a value to continue unfolding from.
-- |
-- | Note that, in order to provide an `Unfoldable1 t` instance, `t` need not
-- | be a type which is guaranteed to be non-empty. For example, the fact that
-- | lists can be empty does not prevent us from providing an
-- | `Unfoldable1 List` instance. However, the result of `unfoldr1` should
-- | always be non-empty.
-- |
-- | Every type which has an `Unfoldable` instance can be given an
-- | `Unfoldable1` instance (and, in fact, is required to, because
-- | `Unfoldable1` is a superclass of `Unfoldable`). However, there are types
-- | which have `Unfoldable1` instances but cannot have `Unfoldable` instances.
-- | In particular, types which are guaranteed to be non-empty, such as
-- | `NonEmptyList`, cannot be given `Unfoldable` instances.
-- |
-- | The utility of this class, then, is that it provides an `Unfoldable`-like
-- | interface while still permitting instances for guaranteed-non-empty types
-- | like `NonEmptyList`.
class Unfoldable1 t where
  unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> t a

instance unfoldable1Array :: Unfoldable1 Array where
  unfoldr1 = unfoldr1ArrayImpl isNothing (unsafePartial fromJust) fst snd

instance unfoldable1Maybe :: Unfoldable1 Maybe where
  unfoldr1 f b = Just (fst (f b))

foreign import unfoldr1ArrayImpl
  :: forall a b
   . (forall x. Maybe x -> Boolean)
  -> (forall x. Maybe x -> x)
  -> (forall x y. Tuple x y -> x)
  -> (forall x y. Tuple x y -> y)
  -> (b -> Tuple a (Maybe b))
  -> b
  -> Array a

-- | Replicate a value `n` times. At least one value will be produced, so values
-- | `n` less than 1 will be treated as 1.
-- |
-- | ``` purescript
-- | replicate1 2 "foo" == (NEL.cons "foo" (NEL.singleton "foo") :: NEL.NonEmptyList String)
-- | replicate1 0 "foo" == (NEL.singleton "foo" :: NEL.NonEmptyList String)
-- | ```
replicate1 :: forall f a. Unfoldable1 f => Int -> a -> f a
replicate1 n v = unfoldr1 step (n - 1)
  where
    step :: Int -> Tuple a (Maybe Int)
    step i
      | i <= 0 = Tuple v Nothing
      | otherwise = Tuple v (Just (i - 1))

-- | Perform an `Apply` action `n` times (at least once, so values `n` less
-- | than 1 will be treated as 1), and accumulate the results.
-- |
-- | ``` purescript
-- | > replicate1A 2 (randomInt 1 10) :: Effect (NEL.NonEmptyList Int)
-- | (NonEmptyList (NonEmpty 8 (2 : Nil)))
-- | > replicate1A 0 (randomInt 1 10) :: Effect (NEL.NonEmptyList Int)
-- | (NonEmptyList (NonEmpty 4 Nil))
-- | ```
replicate1A
  :: forall m f a
   . Apply m
  => Unfoldable1 f
  => Traversable1 f
  => Int
  -> m a
  -> m (f a)
replicate1A n m = sequence1 (replicate1 n m)

-- | Contain a single value. For example:
-- |
-- | ``` purescript
-- | singleton "foo" == (NEL.singleton "foo" :: NEL.NonEmptyList String)
-- | ```
singleton :: forall f a. Unfoldable1 f => a -> f a
singleton = replicate1 1

-- | Create an `Unfoldable1` containing a range of values, including both
-- | endpoints.
-- |
-- | ``` purescript
-- | range 0 0 == (NEL.singleton 0 :: NEL.NonEmptyList Int)
-- | range 1 2 == (NEL.cons 1 (NEL.singleton 2) :: NEL.NonEmptyList Int)
-- | range 2 0 == (NEL.cons 2 (NEL.cons 1 (NEL.singleton 0)) :: NEL.NonEmptyList Int)
-- | ```
range :: forall f. Unfoldable1 f => Int -> Int -> f Int
range start end =
  let delta = if end >= start then 1 else -1 in unfoldr1 (go delta) start
  where
    go delta i =
      let i' = i + delta
      in Tuple i (if i == end then Nothing else Just i')

-- | Create an `Unfoldable1` by repeated application of a function to a seed value.
-- | For example:
-- |
-- | ``` purescript
-- | (iterateN 5 (_ + 1) 0 :: Array Int) == [0, 1, 2, 3, 4]
-- | (iterateN 5 (_ + 1) 0 :: NonEmptyArray Int) == NonEmptyArray [0, 1, 2, 3, 4]
-- |
-- | (iterateN 0 (_ + 1) 0 :: Array Int) == [0]
-- | (iterateN 0 (_ + 1) 0 :: NonEmptyArray Int) == NonEmptyArray [0]
-- | ```
iterateN :: forall f a. Unfoldable1 f => Int -> (a -> a) -> a -> f a
iterateN n f s = unfoldr1 go $ Tuple s (n - 1)
  where
  go (Tuple x n') = Tuple x
    if n' > 0 then Just $ Tuple (f x) $ n' - 1
    else Nothing
