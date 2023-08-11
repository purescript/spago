module Control.Alt
  ( class Alt, alt, (<|>)
  , module Data.Functor
  ) where

import Data.Functor (class Functor, map, void, ($>), (<#>), (<$), (<$>))
import Data.Semigroup (append)

-- | The `Alt` type class identifies an associative operation on a type
-- | constructor.  It is similar to `Semigroup`, except that it applies to
-- | types of kind `* -> *`, like `Array` or `List`, rather than concrete types
-- | `String` or `Number`.
-- |
-- | `Alt` instances are required to satisfy the following laws:
-- |
-- | - Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
-- | - Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`
-- |
-- | For example, the `Array` (`[]`) type is an instance of `Alt`, where
-- | `(<|>)` is defined to be concatenation.
-- |
-- | A common use case is to select the first "valid" item, or, if all items
-- | are "invalid", the last "invalid" item.
-- |
-- | For example:
-- |
-- | ```purescript
-- | import Control.Alt ((<|>))
-- | import Data.Maybe (Maybe(..)
-- | import Data.Either (Either(..))
-- |
-- | Nothing <|> Just 1 <|> Just 2 == Just 1
-- | Left "err" <|> Right 1 <|> Right 2 == Right 1
-- | Left "err 1" <|> Left "err 2" <|> Left "err 3" == Left "err 3"
-- | ```
class Functor f <= Alt f where
  alt :: forall a. f a -> f a -> f a

infixr 3 alt as <|>

instance altArray :: Alt Array where
  alt = append
