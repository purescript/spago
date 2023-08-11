-- | This module provides functions for working with object properties
-- | of Javascript objects.

module Foreign.Keys
  ( keys
  ) where

import Prelude

import Foreign (Foreign, ForeignError(..), typeOf, isUndefined, isNull, fail)
import Control.Monad.Except (ExceptT)
import Data.List.NonEmpty (NonEmptyList)

foreign import unsafeKeys :: Foreign -> Array String

-- | Get an array of the properties defined on a foreign value
keys :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m (Array String)
keys value
  | isNull value = fail $ TypeMismatch "object" "null"
  | isUndefined value = fail $ TypeMismatch "object" "undefined"
  | typeOf value == "object" = pure $ unsafeKeys value
  | otherwise = fail $ TypeMismatch "object" (typeOf value)
