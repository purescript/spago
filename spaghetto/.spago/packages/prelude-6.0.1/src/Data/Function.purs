module Data.Function
  ( flip
  , const
  , apply
  , ($)
  , applyFlipped
  , (#)
  , applyN
  , on
  , module Control.Category
  ) where

import Control.Category (identity, compose, (<<<), (>>>))
import Data.Boolean (otherwise)
import Data.Ord ((<=))
import Data.Ring ((-))

-- | Given a function that takes two arguments, applies the arguments
-- | to the function in a swapped order.
-- |
-- | ```purescript
-- | flip append "1" "2" == append "2" "1" == "21"
-- |
-- | const 1 "two" == 1
-- |
-- | flip const 1 "two" == const "two" 1 == "two"
-- | ```
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- | Returns its first argument and ignores its second.
-- |
-- | ```purescript
-- | const 1 "hello" = 1
-- | ```
-- |
-- | It can also be thought of as creating a function that ignores its argument:
-- |
-- | ```purescript
-- | const 1 = \_ -> 1
-- | ```
const :: forall a b. a -> b -> a
const a _ = a

-- | Applies a function to an argument. This is primarily used as the operator
-- | `($)` which allows parentheses to be omitted in some cases, or as a
-- | natural way to apply a chain of composed functions to a value.
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

-- | Applies a function to an argument: the reverse of `(#)`.
-- |
-- | ```purescript
-- | length $ groupBy productCategory $ filter isInStock $ products
-- | ```
-- |
-- | is equivalent to:
-- |
-- | ```purescript
-- | length (groupBy productCategory (filter isInStock products))
-- | ```
-- |
-- | Or another alternative equivalent, applying chain of composed functions to
-- | a value:
-- |
-- | ```purescript
-- | length <<< groupBy productCategory <<< filter isInStock $ products
-- | ```
infixr 0 apply as $

-- | Applies an argument to a function. This is primarily used as the `(#)`
-- | operator, which allows parentheses to be omitted in some cases, or as a
-- | natural way to apply a value to a chain of composed functions.
applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped x f = f x

-- | Applies an argument to a function: the reverse of `($)`.
-- |
-- | ```purescript
-- | products # filter isInStock # groupBy productCategory # length
-- | ```
-- |
-- | is equivalent to:
-- |
-- | ```purescript
-- | length (groupBy productCategory (filter isInStock products))
-- | ```
-- |
-- | Or another alternative equivalent, applying a value to a chain of composed
-- | functions:
-- |
-- | ```purescript
-- | products # filter isInStock >>> groupBy productCategory >>> length
-- | ```
infixl 1 applyFlipped as #

-- | `applyN f n` applies the function `f` to its argument `n` times.
-- |
-- | If n is less than or equal to 0, the function is not applied.
-- |
-- | ```purescript
-- | applyN (_ + 1) 10 0 == 10
-- | ```
applyN :: forall a. (a -> a) -> Int -> a -> a
applyN f = go
  where
  go n acc
    | n <= 0 = acc
    | otherwise = go (n - 1) (f acc)

-- | The `on` function is used to change the domain of a binary operator.
-- |
-- | For example, we can create a function which compares two records based on the values of their `x` properties:
-- |
-- | ```purescript
-- | compareX :: forall r. { x :: Number | r } -> { x :: Number | r } -> Ordering
-- | compareX = compare `on` _.x
-- | ```
on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = g x `f` g y
