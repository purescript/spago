module Data.Maybe where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.Plus (class Plus)

import Data.Eq (class Eq1)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord1)

-- | The `Maybe` type is used to represent optional values and can be seen as
-- | something like a type-safe `null`, where `Nothing` is `null` and `Just x`
-- | is the non-null value `x`.
data Maybe a = Nothing | Just a

-- | The `Functor` instance allows functions to transform the contents of a
-- | `Just` with the `<$>` operator:
-- |
-- | ``` purescript
-- | f <$> Just x == Just (f x)
-- | ```
-- |
-- | `Nothing` values are left untouched:
-- |
-- | ``` purescript
-- | f <$> Nothing == Nothing
-- | ```
instance functorMaybe :: Functor Maybe where
  map fn (Just x) = Just (fn x)
  map _  _        = Nothing

-- | The `Apply` instance allows functions contained within a `Just` to
-- | transform a value contained within a `Just` using the `apply` operator:
-- |
-- | ``` purescript
-- | Just f <*> Just x == Just (f x)
-- | ```
-- |
-- | `Nothing` values are left untouched:
-- |
-- | ``` purescript
-- | Just f <*> Nothing == Nothing
-- | Nothing <*> Just x == Nothing
-- | ```
-- |
-- | Combining `Functor`'s `<$>` with `Apply`'s `<*>` can be used transform a
-- | pure function to take `Maybe`-typed arguments so `f :: a -> b -> c`
-- | becomes `f :: Maybe a -> Maybe b -> Maybe c`:
-- |
-- | ``` purescript
-- | f <$> Just x <*> Just y == Just (f x y)
-- | ```
-- |
-- | The `Nothing`-preserving behaviour of both operators means the result of
-- | an expression like the above but where any one of the values is `Nothing`
-- | means the whole result becomes `Nothing` also:
-- |
-- | ``` purescript
-- | f <$> Nothing <*> Just y == Nothing
-- | f <$> Just x <*> Nothing == Nothing
-- | f <$> Nothing <*> Nothing == Nothing
-- | ```
instance applyMaybe :: Apply Maybe where
  apply (Just fn) x = fn <$> x
  apply Nothing   _ = Nothing

-- | The `Applicative` instance enables lifting of values into `Maybe` with the
-- | `pure` function:
-- |
-- | ``` purescript
-- | pure x :: Maybe _ == Just x
-- | ```
-- |
-- | Combining `Functor`'s `<$>` with `Apply`'s `<*>` and `Applicative`'s
-- | `pure` can be used to pass a mixture of `Maybe` and non-`Maybe` typed
-- | values to a function that does not usually expect them, by using `pure`
-- | for any value that is not already `Maybe` typed:
-- |
-- | ``` purescript
-- | f <$> Just x <*> pure y == Just (f x y)
-- | ```
-- |
-- | Even though `pure = Just` it is recommended to use `pure` in situations
-- | like this as it allows the choice of `Applicative` to be changed later
-- | without having to go through and replace `Just` with a new constructor.
instance applicativeMaybe :: Applicative Maybe where
  pure = Just

-- | The `Alt` instance allows for a choice to be made between two `Maybe`
-- | values with the `<|>` operator, where the first `Just` encountered
-- | is taken.
-- |
-- | ``` purescript
-- | Just x <|> Just y == Just x
-- | Nothing <|> Just y == Just y
-- | Nothing <|> Nothing == Nothing
-- | ```
instance altMaybe :: Alt Maybe where
  alt Nothing r = r
  alt l       _ = l

-- | The `Plus` instance provides a default `Maybe` value:
-- |
-- | ``` purescript
-- | empty :: Maybe _ == Nothing
-- | ```
instance plusMaybe :: Plus Maybe where
  empty = Nothing

-- | The `Alternative` instance guarantees that there are both `Applicative` and
-- | `Plus` instances for `Maybe`.
instance alternativeMaybe :: Alternative Maybe

-- | The `Bind` instance allows sequencing of `Maybe` values and functions that
-- | return a `Maybe` by using the `>>=` operator:
-- |
-- | ``` purescript
-- | Just x >>= f = f x
-- | Nothing >>= f = Nothing
-- | ```
instance bindMaybe :: Bind Maybe where
  bind (Just x) k = k x
  bind Nothing  _ = Nothing

-- | The `Monad` instance guarantees that there are both `Applicative` and
-- | `Bind` instances for `Maybe`. This also enables the `do` syntactic sugar:
-- |
-- | ``` purescript
-- | do
-- |   x' <- x
-- |   y' <- y
-- |   pure (f x' y')
-- | ```
-- |
-- | Which is equivalent to:
-- |
-- | ``` purescript
-- | x >>= (\x' -> y >>= (\y' -> pure (f x' y')))
-- | ```
-- |
-- | Which is equivalent to:
-- |
-- | ``` purescript
-- | case x of
-- |   Nothing -> Nothing
-- |   Just x' -> case y of
-- |     Nothing -> Nothing
-- |     Just y' -> Just (f x' y')
-- | ```
instance monadMaybe :: Monad Maybe

-- | The `Extend` instance allows sequencing of `Maybe` values and functions
-- | that accept a `Maybe a` and return a non-`Maybe` result using the
-- | `<<=` operator.
-- |
-- | ``` purescript
-- | f <<= Nothing = Nothing
-- | f <<= x = Just (f x)
-- | ```
instance extendMaybe :: Extend Maybe where
  extend _ Nothing  = Nothing
  extend f x        = Just (f x)

instance invariantMaybe :: Invariant Maybe where
  imap = imapF

-- | The `Semigroup` instance enables use of the operator `<>` on `Maybe` values
-- | whenever there is a `Semigroup` instance for the type the `Maybe` contains.
-- | The exact behaviour of `<>` depends on the "inner" `Semigroup` instance,
-- | but generally captures the notion of appending or combining things.
-- |
-- | ``` purescript
-- | Just x <> Just y = Just (x <> y)
-- | Just x <> Nothing = Just x
-- | Nothing <> Just y = Just y
-- | Nothing <> Nothing = Nothing
-- | ```
instance semigroupMaybe :: Semigroup a => Semigroup (Maybe a) where
  append Nothing y = y
  append x Nothing = x
  append (Just x) (Just y) = Just (x <> y)

instance monoidMaybe :: Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

instance semiringMaybe :: Semiring a => Semiring (Maybe a) where
  zero = Nothing
  one = Just one

  add Nothing y = y
  add x Nothing = x
  add (Just x) (Just y) = Just (add x y)

  mul x y = mul <$> x <*> y

-- | The `Eq` instance allows `Maybe` values to be checked for equality with
-- | `==` and inequality with `/=` whenever there is an `Eq` instance for the
-- | type the `Maybe` contains.
derive instance eqMaybe :: Eq a => Eq (Maybe a)

instance eq1Maybe :: Eq1 Maybe where eq1 = eq

-- | The `Ord` instance allows `Maybe` values to be compared with
-- | `compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
-- | the type the `Maybe` contains.
-- |
-- | `Nothing` is considered to be less than any `Just` value.
derive instance ordMaybe :: Ord a => Ord (Maybe a)

instance ord1Maybe :: Ord1 Maybe where compare1 = compare

instance boundedMaybe :: Bounded a => Bounded (Maybe a) where
  top = Just top
  bottom = Nothing

-- | The `Show` instance allows `Maybe` values to be rendered as a string with
-- | `show` whenever there is an `Show` instance for the type the `Maybe`
-- | contains.
instance showMaybe :: Show a => Show (Maybe a) where
  show (Just x) = "(Just " <> show x <> ")"
  show Nothing  = "Nothing"

derive instance genericMaybe :: Generic (Maybe a) _

-- | Takes a default value, a function, and a `Maybe` value. If the `Maybe`
-- | value is `Nothing` the default value is returned, otherwise the function
-- | is applied to the value inside the `Just` and the result is returned.
-- |
-- | ``` purescript
-- | maybe x f Nothing == x
-- | maybe x f (Just y) == f y
-- | ```
maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just a) = f a

-- | Similar to `maybe` but for use in cases where the default value may be
-- | expensive to compute. As PureScript is not lazy, the standard `maybe` has
-- | to evaluate the default value before returning the result, whereas here
-- | the value is only computed when the `Maybe` is known to be `Nothing`.
-- |
-- | ``` purescript
-- | maybe' (\_ -> x) f Nothing == x
-- | maybe' (\_ -> x) f (Just y) == f y
-- | ```
maybe' :: forall a b. (Unit -> b) -> (a -> b) -> Maybe a -> b
maybe' g _ Nothing = g unit
maybe' _ f (Just a) = f a

-- | Takes a default value, and a `Maybe` value. If the `Maybe` value is
-- | `Nothing` the default value is returned, otherwise the value inside the
-- | `Just` is returned.
-- |
-- | ``` purescript
-- | fromMaybe x Nothing == x
-- | fromMaybe x (Just y) == y
-- | ```
fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe a = maybe a identity

-- | Similar to `fromMaybe` but for use in cases where the default value may be
-- | expensive to compute. As PureScript is not lazy, the standard `fromMaybe`
-- | has to evaluate the default value before returning the result, whereas here
-- | the value is only computed when the `Maybe` is known to be `Nothing`.
-- |
-- | ``` purescript
-- | fromMaybe' (\_ -> x) Nothing == x
-- | fromMaybe' (\_ -> x) (Just y) == y
-- | ```
fromMaybe' :: forall a. (Unit -> a) -> Maybe a -> a
fromMaybe' a = maybe' a identity

-- | Returns `true` when the `Maybe` value was constructed with `Just`.
isJust :: forall a. Maybe a -> Boolean
isJust = maybe false (const true)

-- | Returns `true` when the `Maybe` value is `Nothing`.
isNothing :: forall a. Maybe a -> Boolean
isNothing = maybe true (const false)

-- | A partial function that extracts the value from the `Just` data
-- | constructor. Passing `Nothing` to `fromJust` will throw an error at
-- | runtime.
fromJust :: forall a. Partial => Maybe a -> a
fromJust (Just x) = x

-- | One or none.
-- |
-- | ```purescript
-- | optional empty = pure Nothing
-- | ```
-- |
-- | The behaviour of `optional (pure x)` depends on whether the `Alt` instance
-- | satisfy the left catch law (`pure a <|> b = pure a`).
-- |
-- | `Either e` does:
-- |
-- | ```purescript
-- | optional (Right x) = Right (Just x)
-- | ```
-- |
-- | But `Array` does not:
-- |
-- | ```purescript
-- | optional [x] = [Just x, Nothing]
-- | ```
optional :: forall f a. Alt f => Applicative f => f a -> f (Maybe a)
optional a = map Just a <|> pure Nothing
