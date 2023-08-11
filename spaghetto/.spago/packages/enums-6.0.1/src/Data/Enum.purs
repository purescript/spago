module Data.Enum
  ( class Enum, succ, pred
  , class BoundedEnum, cardinality, toEnum, fromEnum
  , toEnumWithDefaults
  , Cardinality(..)
  , enumFromTo
  , enumFromThenTo
  , upFrom
  , upFromIncluding
  , downFrom
  , downFromIncluding
  , defaultSucc
  , defaultPred
  , defaultCardinality
  , defaultToEnum
  , defaultFromEnum
  ) where

import Prelude

import Control.MonadPlus (guard)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, singleton, unfoldr)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Partial.Unsafe (unsafePartial)

-- | Type class for enumerations.
-- |
-- | Laws:
-- | - Successor: `all (a < _) (succ a)`
-- | - Predecessor: `all (_ < a) (pred a)`
-- | - Succ retracts pred: `pred >=> succ >=> pred = pred`
-- | - Pred retracts succ: `succ >=> pred >=> succ = succ`
-- | - Non-skipping succ: `b <= a || any (_ <= b) (succ a)`
-- | - Non-skipping pred: `a <= b || any (b <= _) (pred a)`
-- |
-- | The retraction laws can intuitively be understood as saying that `succ` is
-- | the opposite of `pred`; if you apply `succ` and then `pred` to something,
-- | you should end up with what you started with (although of course this
-- | doesn't apply if you tried to `succ` the last value in an enumeration and
-- | therefore got `Nothing` out).
-- |
-- | The non-skipping laws can intuitively be understood as saying that `succ`
-- | shouldn't skip over any elements of your type. For example, _without_ the
-- | non-skipping laws, it would be permissible to write an `Enum Int` instance
-- | where `succ x = Just (x+2)`, and similarly `pred x = Just (x-2)`.
class Ord a <= Enum a where
  succ :: a -> Maybe a
  pred :: a -> Maybe a

instance enumBoolean :: Enum Boolean where
  succ false = Just true
  succ _ = Nothing
  pred true = Just false
  pred _= Nothing

instance enumInt :: Enum Int where
  succ n = if n < top then Just (n + 1) else Nothing
  pred n = if n > bottom then Just (n - 1) else Nothing

instance enumChar :: Enum Char where
  succ = defaultSucc charToEnum toCharCode
  pred = defaultPred charToEnum toCharCode

instance enumUnit :: Enum Unit where
  succ = const Nothing
  pred = const Nothing

instance enumOrdering :: Enum Ordering where
  succ LT = Just EQ
  succ EQ = Just GT
  succ GT = Nothing
  pred LT = Nothing
  pred EQ = Just LT
  pred GT = Just EQ

instance enumMaybe :: BoundedEnum a => Enum (Maybe a) where
  succ Nothing = Just (Just bottom)
  succ (Just a) = Just <$> succ a
  pred Nothing = Nothing
  pred (Just a) = Just (pred a)

instance enumEither :: (BoundedEnum a, BoundedEnum b) => Enum (Either a b) where
  succ (Left a) = maybe (Just (Right bottom)) (Just <<< Left) (succ a)
  succ (Right b) = maybe Nothing (Just <<< Right) (succ b)
  pred (Left a) = maybe Nothing (Just <<< Left) (pred a)
  pred (Right b) = maybe (Just (Left top)) (Just <<< Right) (pred b)

instance enumTuple :: (Enum a, BoundedEnum b) => Enum (Tuple a b) where
  succ (Tuple a b) = maybe (flip Tuple bottom <$> succ a) (Just <<< Tuple a) (succ b)
  pred (Tuple a b) = maybe (flip Tuple top <$> pred a) (Just <<< Tuple a) (pred b)

-- | Type class for finite enumerations.
-- |
-- | This should not be considered a part of a numeric hierarchy, as in Haskell.
-- | Rather, this is a type class for small, ordered sum types with
-- | statically-determined cardinality and the ability to easily compute
-- | successor and predecessor elements like `DayOfWeek`.
-- |
-- | Laws:
-- |
-- | - ```succ bottom >>= succ >>= succ ... succ [cardinality - 1 times] == top```
-- | - ```pred top    >>= pred >>= pred ... pred [cardinality - 1 times] == bottom```
-- | - ```forall a > bottom: pred a >>= succ == Just a```
-- | - ```forall a < top:  succ a >>= pred == Just a```
-- | - ```forall a > bottom: fromEnum <$> pred a = pred (fromEnum a)```
-- | - ```forall a < top:  fromEnum <$> succ a = succ (fromEnum a)```
-- | - ```e1 `compare` e2 == fromEnum e1 `compare` fromEnum e2```
-- | - ```toEnum (fromEnum a) = Just a```
class (Bounded a, Enum a) <= BoundedEnum a where
  cardinality :: Cardinality a
  toEnum :: Int -> Maybe a
  fromEnum :: a -> Int

instance boundedEnumBoolean :: BoundedEnum Boolean where
  cardinality = Cardinality 2
  toEnum 0 = Just false
  toEnum 1 = Just true
  toEnum _ = Nothing
  fromEnum false = 0
  fromEnum true = 1

instance boundedEnumChar :: BoundedEnum Char where
  cardinality = Cardinality (toCharCode top - toCharCode bottom)
  toEnum = charToEnum
  fromEnum = toCharCode

instance boundedEnumUnit :: BoundedEnum Unit where
  cardinality = Cardinality 1
  toEnum 0 = Just unit
  toEnum _ = Nothing
  fromEnum = const 0

instance boundedEnumOrdering :: BoundedEnum Ordering where
  cardinality = Cardinality 3
  toEnum 0 = Just LT
  toEnum 1 = Just EQ
  toEnum 2 = Just GT
  toEnum _ = Nothing
  fromEnum LT = 0
  fromEnum EQ = 1
  fromEnum GT = 2

-- | Like `toEnum` but returns the first argument if `x` is less than
-- | `fromEnum bottom` and the second argument if `x` is greater than
-- | `fromEnum top`.
-- |
-- | ``` purescript
-- | toEnumWithDefaults False True (-1) -- False
-- | toEnumWithDefaults False True 0    -- False
-- | toEnumWithDefaults False True 1    -- True
-- | toEnumWithDefaults False True 2    -- True
-- | ```
toEnumWithDefaults :: forall a. BoundedEnum a => a -> a -> Int -> a
toEnumWithDefaults low high x = case toEnum x of
  Just enum -> enum
  Nothing -> if x < fromEnum (bottom :: a) then low else high

-- | A type for the size of finite enumerations.
newtype Cardinality :: forall k. k -> Type
newtype Cardinality a = Cardinality Int

type role Cardinality representational

derive instance newtypeCardinality :: Newtype (Cardinality a) _
derive newtype instance eqCardinality :: Eq (Cardinality a)
derive newtype instance ordCardinality :: Ord (Cardinality a)

instance showCardinality :: Show (Cardinality a) where
  show (Cardinality n) = "(Cardinality " <> show n <> ")"

-- | Returns a contiguous sequence of elements from the first value to the
-- | second value (inclusive).
-- |
-- | ``` purescript
-- | enumFromTo 0 3 = [0, 1, 2, 3]
-- | enumFromTo 'c' 'a' = ['c', 'b', 'a']
-- | ```
-- |
-- | The example shows `Array` return values, but the result can be any type
-- | with an `Unfoldable1` instance.
enumFromTo :: forall a u. Enum a => Unfoldable1 u => a -> a -> u a
enumFromTo = case _, _ of
  from, to
    | from == to -> singleton from
    | from < to -> unfoldr1 (go succ (<=) to) from
    | otherwise -> unfoldr1 (go pred (>=) to) from
  where
    go step op to a = Tuple a (step a >>= \a' -> guard (a' `op` to) $> a')

-- | Returns a sequence of elements from the first value, taking steps
-- | according to the difference between the first and second value, up to
-- | (but not exceeding) the third value.
-- |
-- | ``` purescript
-- | enumFromThenTo 0 2 6 = [0, 2, 4, 6]
-- | enumFromThenTo 0 3 5 = [0, 3]
-- | ```
-- |
-- | Note that there is no `BoundedEnum` instance for integers, they're just
-- | being used here for illustrative purposes to help clarify the behaviour.
-- |
-- | The example shows `Array` return values, but the result can be any type
-- | with an `Unfoldable1` instance.
enumFromThenTo :: forall f a. Unfoldable f => Functor f => BoundedEnum a => a -> a -> a -> f a
enumFromThenTo = unsafePartial \a b c ->
  let
    a' = fromEnum a
    b' = fromEnum b
    c' = fromEnum c
  in
    (toEnum >>> fromJust) <$> unfoldr (go (b' - a') c') a'
  where
    go step to e
      | e <= to = Just (Tuple e (e + step))
      | otherwise = Nothing

-- | Produces all successors of an `Enum` value, excluding the start value.
upFrom :: forall a u. Enum a => Unfoldable u => a -> u a
upFrom = unfoldr (map diag <<< succ)

-- | Produces all successors of an `Enum` value, including the start value.
-- |
-- | `upFromIncluding bottom` will return all values in an `Enum`.
upFromIncluding :: ∀ a u. Enum a => Unfoldable1 u => a -> u a
upFromIncluding = unfoldr1 (Tuple <*> succ)

-- | Produces all predecessors of an `Enum` value, excluding the start value.
downFrom :: forall a u. Enum a => Unfoldable u => a -> u a
downFrom = unfoldr (map diag <<< pred)

-- | Produces all predecessors of an `Enum` value, including the start value.
-- |
-- | `downFromIncluding top` will return all values in an `Enum`, in reverse
-- | order.
downFromIncluding :: forall a u. Enum a => Unfoldable1 u => a -> u a
downFromIncluding = unfoldr1 (Tuple <*> pred)

-- | Provides a default implementation for `succ`, given a function that maps
-- | integers to values in the `Enum`, and a function that maps values in the
-- | `Enum` back to integers. The integer mapping must agree in both directions
-- | for this to implement a law-abiding `succ`.
-- |
-- | If a `BoundedEnum` instance exists for `a`, the `toEnum` and `fromEnum`
-- | functions can be used here:
-- |
-- | ``` purescript
-- | succ = defaultSucc toEnum fromEnum
-- | ```
defaultSucc :: forall a. (Int -> Maybe a) -> (a -> Int) -> a -> Maybe a
defaultSucc toEnum' fromEnum' a = toEnum' (fromEnum' a + 1)

-- | Provides a default implementation for `pred`, given a function that maps
-- | integers to values in the `Enum`, and a function that maps values in the
-- | `Enum` back to integers. The integer mapping must agree in both directions
-- | for this to implement a law-abiding `pred`.
-- |
-- | If a `BoundedEnum` instance exists for `a`, the `toEnum` and `fromEnum`
-- | functions can be used here:
-- |
-- | ``` purescript
-- | pred = defaultPred toEnum fromEnum
-- | ```
defaultPred :: forall a. (Int -> Maybe a) -> (a -> Int) -> a -> Maybe a
defaultPred toEnum' fromEnum' a = toEnum' (fromEnum' a - 1)

-- | Provides a default implementation for `cardinality`.
-- |
-- | Runs in `O(n)` where `n` is `fromEnum top`
defaultCardinality :: forall a. Bounded a => Enum a => Cardinality a
defaultCardinality = Cardinality $ go 1 (bottom :: a) where
  go i x =
    case succ x of
      Just x' -> go (i + 1) x'
      Nothing -> i

-- | Provides a default implementation for `toEnum`.
-- |
-- | - Assumes `fromEnum bottom = 0`.
-- | - Cannot be used in conjuction with `defaultSucc`.
-- |
-- | Runs in `O(n)` where `n` is `fromEnum a`.
defaultToEnum :: forall a. Bounded a => Enum a => Int -> Maybe a
defaultToEnum i' =
  if i' < 0
    then Nothing
    else go i' bottom
  where
  go i x =
    if i == 0
      then Just x
      -- We avoid using >>= here because it foils tail-call optimization
      else case succ x of
              Just x' -> go (i - 1) x'
              Nothing -> Nothing

-- | Provides a default implementation for `fromEnum`.
-- |
-- | - Assumes `toEnum 0 = Just bottom`.
-- | - Cannot be used in conjuction with `defaultPred`.
-- |
-- | Runs in `O(n)` where `n` is `fromEnum a`.
defaultFromEnum :: forall a. Enum a => a -> Int
defaultFromEnum = go 0 where
  go i x =
    case pred x of
      Just x' -> go (i + 1) x'
      Nothing -> i

diag :: forall a. a -> Tuple a a
diag a = Tuple a a

charToEnum :: Int -> Maybe Char
charToEnum n | n >= toCharCode bottom && n <= toCharCode top = Just (fromCharCode n)
charToEnum _ = Nothing

foreign import toCharCode :: Char -> Int
foreign import fromCharCode :: Int -> Char
