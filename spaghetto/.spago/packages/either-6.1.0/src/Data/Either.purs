module Data.Either where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Extend (class Extend)
import Data.Eq (class Eq1)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Ord (class Ord1)

-- | The `Either` type is used to represent a choice between two types of value.
-- |
-- | A common use case for `Either` is error handling, where `Left` is used to
-- | carry an error value and `Right` is used to carry a success value.
data Either a b = Left a | Right b

-- | The `Functor` instance allows functions to transform the contents of a
-- | `Right` with the `<$>` operator:
-- |
-- | ``` purescript
-- | f <$> Right x == Right (f x)
-- | ```
-- |
-- | `Left` values are untouched:
-- |
-- | ``` purescript
-- | f <$> Left y == Left y
-- | ```
derive instance functorEither :: Functor (Either a)

derive instance genericEither :: Generic (Either a b) _

instance invariantEither :: Invariant (Either a) where
  imap = imapF

-- | The `Apply` instance allows functions contained within a `Right` to
-- | transform a value contained within a `Right` using the `(<*>)` operator:
-- |
-- | ``` purescript
-- | Right f <*> Right x == Right (f x)
-- | ```
-- |
-- | `Left` values are left untouched:
-- |
-- | ``` purescript
-- | Left f <*> Right x == Left f
-- | Right f <*> Left y == Left y
-- | ```
-- |
-- | Combining `Functor`'s `<$>` with `Apply`'s `<*>` can be used to transform a
-- | pure function to take `Either`-typed arguments so `f :: a -> b -> c`
-- | becomes `f :: Either l a -> Either l b -> Either l c`:
-- |
-- | ``` purescript
-- | f <$> Right x <*> Right y == Right (f x y)
-- | ```
-- |
-- | The `Left`-preserving behaviour of both operators means the result of
-- | an expression like the above but where any one of the values is `Left`
-- | means the whole result becomes `Left` also, taking the first `Left` value
-- | found:
-- |
-- | ``` purescript
-- | f <$> Left x <*> Right y == Left x
-- | f <$> Right x <*> Left y == Left y
-- | f <$> Left x <*> Left y == Left x
-- | ```
instance applyEither :: Apply (Either e) where
  apply (Left e) _ = Left e
  apply (Right f) r = f <$> r

-- | The `Applicative` instance enables lifting of values into `Either` with the
-- | `pure` function:
-- |
-- | ``` purescript
-- | pure x :: Either _ _ == Right x
-- | ```
-- |
-- | Combining `Functor`'s `<$>` with `Apply`'s `<*>` and `Applicative`'s
-- | `pure` can be used to pass a mixture of `Either` and non-`Either` typed
-- | values to a function that does not usually expect them, by using `pure`
-- | for any value that is not already `Either` typed:
-- |
-- | ``` purescript
-- | f <$> Right x <*> pure y == Right (f x y)
-- | ```
-- |
-- | Even though `pure = Right` it is recommended to use `pure` in situations
-- | like this as it allows the choice of `Applicative` to be changed later
-- | without having to go through and replace `Right` with a new constructor.
instance applicativeEither :: Applicative (Either e) where
  pure = Right

-- | The `Alt` instance allows for a choice to be made between two `Either`
-- | values with the `<|>` operator, where the first `Right` encountered
-- | is taken.
-- |
-- | ``` purescript
-- | Right x <|> Right y == Right x
-- | Left x <|> Right y == Right y
-- | Left x <|> Left y == Left y
-- | ```
instance altEither :: Alt (Either e) where
  alt (Left _) r = r
  alt l        _ = l

-- | The `Bind` instance allows sequencing of `Either` values and functions that
-- | return an `Either` by using the `>>=` operator:
-- |
-- | ``` purescript
-- | Left x >>= f = Left x
-- | Right x >>= f = f x
-- | ```
-- |
-- | `Either`'s "do notation" can be understood to work like this:
-- | ``` purescript
-- | x :: forall e a. Either e a
-- | x = --
-- |
-- | y :: forall e b. Either e b
-- | y = --
-- |
-- | foo :: forall e a. (a -> b -> c) -> Either e c
-- | foo f = do
-- |   x' <- x
-- |   y' <- y
-- |   pure (f x' y')
-- | ```
-- |
-- | ...which is equivalent to...
-- |
-- | ``` purescript
-- | x >>= (\x' -> y >>= (\y' -> pure (f x' y')))
-- | ```
-- |
-- | ...and is the same as writing...
-- |
-- | ```
-- | foo :: forall e a. (a -> b -> c) -> Either e c
-- | foo f = case x of
-- |   Left e ->
-- |     Left e
-- |   Right x -> case y of
-- |     Left e ->
-- |       Left e
-- |     Right y ->
-- |       Right (f x y)
-- | ```
instance bindEither :: Bind (Either e) where
  bind = either (\e _ -> Left e) (\a f -> f a)

-- | The `Monad` instance guarantees that there are both `Applicative` and
-- | `Bind` instances for `Either`.
instance monadEither :: Monad (Either e)

-- | The `Extend` instance allows sequencing of `Either` values and functions
-- | that accept an `Either` and return a non-`Either` result using the
-- | `<<=` operator.
-- |
-- | ``` purescript
-- | f <<= Left x = Left x
-- | f <<= Right x = Right (f (Right x))
-- | ```
instance extendEither :: Extend (Either e) where
  extend _ (Left y)  = Left y
  extend f x         = Right (f x)

-- | The `Show` instance allows `Either` values to be rendered as a string with
-- | `show` whenever there is an `Show` instance for both type the `Either` can
-- | contain.
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show (Left x) = "(Left " <> show x <> ")"
  show (Right y) = "(Right " <> show y <> ")"

-- | The `Eq` instance allows `Either` values to be checked for equality with
-- | `==` and inequality with `/=` whenever there is an `Eq` instance for both
-- | types the `Either` can contain.
derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)

derive instance eq1Either :: Eq a => Eq1 (Either a)

-- | The `Ord` instance allows `Either` values to be compared with
-- | `compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
-- | both types the `Either` can contain.
-- |
-- | Any `Left` value is considered to be less than a `Right` value.
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)

derive instance ord1Either :: Ord a => Ord1 (Either a)

instance boundedEither :: (Bounded a, Bounded b) => Bounded (Either a b) where
  top = Right top
  bottom = Left bottom

instance semigroupEither :: (Semigroup b) => Semigroup (Either a b) where
  append x y = append <$> x <*> y

-- | Takes two functions and an `Either` value, if the value is a `Left` the
-- | inner value is applied to the first function, if the value is a `Right`
-- | the inner value is applied to the second function.
-- |
-- | ``` purescript
-- | either f g (Left x) == f x
-- | either f g (Right y) == g y
-- | ```
either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

-- | Combine two alternatives.
choose :: forall m a b. Alt m => m a -> m b -> m (Either a b)
choose a b = Left <$> a <|> Right <$> b

-- | Returns `true` when the `Either` value was constructed with `Left`.
isLeft :: forall a b. Either a b -> Boolean
isLeft = either (const true) (const false)

-- | Returns `true` when the `Either` value was constructed with `Right`.
isRight :: forall a b. Either a b -> Boolean
isRight = either (const false) (const true)

-- | A function that extracts the value from the `Left` data constructor.
-- | The first argument is a default value, which will be returned in the
-- | case where a `Right` is passed to `fromLeft`.
fromLeft :: forall a b. a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft default _ = default

-- | Similar to `fromLeft` but for use in cases where the default value may be
-- | expensive to compute. As PureScript is not lazy, the standard `fromLeft`
-- | has to evaluate the default value before returning the result,
-- | whereas here the value is only computed when the `Either` is known
-- | to be `Right`.
fromLeft' :: forall a b. (Unit -> a) -> Either a b -> a
fromLeft' _ (Left a) = a
fromLeft' default _ = default unit

-- | A function that extracts the value from the `Right` data constructor.
-- | The first argument is a default value, which will be returned in the
-- | case where a `Left` is passed to `fromRight`.
fromRight :: forall a b. b -> Either a b -> b
fromRight _ (Right b) = b
fromRight default _ = default

-- | Similar to `fromRight` but for use in cases where the default value may be
-- | expensive to compute. As PureScript is not lazy, the standard `fromRight`
-- | has to evaluate the default value before returning the result,
-- | whereas here the value is only computed when the `Either` is known
-- | to be `Left`.
fromRight' :: forall a b. (Unit -> b) -> Either a b -> b
fromRight' _ (Right b) = b
fromRight' default _ = default unit

-- | Takes a default and a `Maybe` value, if the value is a `Just`, turn it into
-- | a `Right`, if the value is a `Nothing` use the provided default as a `Left`
-- |
-- | ```purescript
-- | note "default" Nothing = Left "default"
-- | note "default" (Just 1) = Right 1
-- | ```
note :: forall a b. a -> Maybe b -> Either a b
note a = maybe (Left a) Right

-- | Similar to `note`, but for use in cases where the default value may be
-- | expensive to compute.
-- |
-- | ```purescript
-- | note' (\_ -> "default") Nothing = Left "default"
-- | note' (\_ -> "default") (Just 1) = Right 1
-- | ```
note' :: forall a b. (Unit -> a) -> Maybe b -> Either a b
note' f = maybe' (Left <<< f) Right

-- | Turns an `Either` into a `Maybe`, by throwing potential `Left` values away and converting
-- | them into `Nothing`. `Right` values get turned into `Just`s.
-- |
-- | ```purescript
-- | hush (Left "ParseError") = Nothing
-- | hush (Right 42) = Just 42
-- | ```
hush :: forall a b. Either a b -> Maybe b
hush = either (const Nothing) Just

-- | Turns an `Either` into a `Maybe`, by throwing potential `Right` values away and converting
-- | them into `Nothing`. `Left` values get turned into `Just`s.
-- |
-- | ```purescript
-- | blush (Left "ParseError") = Just "Parse Error"
-- | blush (Right 42) = Nothing
-- | ```
blush :: forall a b. Either a b -> Maybe a
blush = either Just (const Nothing)
