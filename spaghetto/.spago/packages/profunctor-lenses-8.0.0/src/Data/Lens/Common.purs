-- | This module defines common lenses and prisms.
module Data.Lens.Common
  ( module Data.Lens.Lens.Tuple
  , module Data.Lens.Lens.Unit
  , module Data.Lens.Prism.Either
  , module Data.Lens.Prism.Maybe
  , simple
  ) where

import Data.Lens.Lens.Tuple (_1, _2, first, second)
import Data.Lens.Lens.Unit (united)
import Data.Lens.Prism.Either (_Left, _Right, left, right)
import Data.Lens.Prism.Maybe (_Just, _Nothing)
import Data.Lens.Types (Optic')

-- | This is useful for when you want to restrict the type of another optic.
-- | For example, suppose you have the following declarations:
-- | ```purescript
-- | newtype X = X Int
-- | derive instance newtypeX :: Newtype X _
-- | ```
-- |
-- | Attempting to view with the `_Newtype` optic:
-- | ```purescript
-- | X 42 ^. _Newtype
-- | ```
-- | Will result in a type error:
-- | ```
-- |  The inferred type
-- |    forall t3 t5. Newtype t3 t5 => Int
-- |  has type variables which are not mentioned in the body of the type.
-- |  Consider adding a type annotation.
-- | ```
-- |
-- | However, if we apply the `simple` function:
-- | ```purescript
-- |  X 42 ^. simple _Newtype
-- | ```
-- | We get the expected result `42`.
simple :: forall p s a. Optic' p s a -> Optic' p s a
simple x = x

