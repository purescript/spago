module Test.Spec.Assertions.String
  ( shouldContain
  , shouldNotContain
  , shouldStartWith
  , shouldEndWith
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.String (Pattern(..), contains)
import Effect.Exception (Error)
import Test.Spec.Assertions (fail)

foreign import _startsWith :: String -> String -> Boolean
foreign import _endsWith :: String -> String -> Boolean

-- | Asserts `string` starts with `prefix`
-- |
-- | ```purescript
-- | string `shouldStartWith` prefix
-- | ```
shouldStartWith :: forall m. MonadThrow Error m => String -> String -> m Unit
shouldStartWith s prefix =
  when (not $ _startsWith prefix s) $
     fail $ show s <> " does not start with " <> show prefix

-- | Asserts `string` ends with `suffix`
-- |
-- | ```purescript
-- | string `shouldEndWith` suffix
-- | ```
shouldEndWith :: forall m. MonadThrow Error m => String -> String -> m Unit
shouldEndWith s suffix =
  when (not $ _endsWith suffix s) $
     fail $ show s <> " does not end with " <> show suffix

-- | Asserts `string` contains `subs`
-- |
-- | ```purescript
-- | string `shouldContain` subs
-- | ```
shouldContain :: forall m. MonadThrow Error m => String -> String -> m Unit
shouldContain s subs =
  when (not $ contains (Pattern subs) s) $
    fail $ show subs <> " ∉ " <> show s

-- | Asserts `string` does not contain `subs`
-- |
-- | ```purescript
-- | string `shouldContain` subs
-- | ```
shouldNotContain :: forall m. MonadThrow Error m => String -> String -> m Unit
shouldNotContain s subs =
  when (contains (Pattern subs) s) $
    fail $ show subs <> " ∈ " <> show s
