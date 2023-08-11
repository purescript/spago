-- | Partial helper functions for working with strict linked lists.
module Data.List.Partial where

import Data.List (List(..))

-- | Get the first element of a non-empty list.
-- |
-- | Running time: `O(1)`.
head :: forall a. Partial => List a -> a
head (Cons x _) = x

-- | Get all but the first element of a non-empty list.
-- |
-- | Running time: `O(1)`
tail :: forall a. Partial => List a -> List a
tail (Cons _ xs) = xs

-- | Get the last element of a non-empty list.
-- |
-- | Running time: `O(n)`
last :: forall a. Partial => List a -> a
last (Cons x Nil) = x
last (Cons _ xs)  = last xs

-- | Get all but the last element of a non-empty list.
-- |
-- | Running time: `O(n)`
init :: forall a. Partial => List a -> List a
init (Cons _ Nil) = Nil
init (Cons x xs)  = Cons x (init xs)
