module Test.Spec.Assertions
  ( AnyShow(..)
  , expectError
  , fail
  , shouldContain
  , shouldEqual
  , shouldNotContain
  , shouldNotEqual
  , shouldNotReturn
  , shouldNotSatisfy
  , shouldReturn
  , shouldSatisfy
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, notElem, elem)
import Data.Newtype (class Newtype)
import Effect.Exception (Error, error)

fail :: forall m. MonadThrow Error m => String -> m Unit
fail = throwError <<< error

foreign import unsafeStringify :: forall a. a -> String

-- | A newtype with an unsafe `Show` instance for any type.
-- | Useful if you want to test a type for which you cannot provide a `Show` instance.
-- | Usage:
-- | ```purescript
-- | (AnyShow $ MyInt 3) `A.shouldEqual` (AnyShow $ MyInt 3)
-- | ```
newtype AnyShow a = AnyShow a
instance Newtype (AnyShow a) a
derive newtype instance (Eq a) => Eq (AnyShow a)
instance Show (AnyShow a) where
  show = unsafeStringify

shouldEqual
  :: forall m t
   . MonadThrow Error m
  => Show t
  => Eq t
  => t
  -> t
  -> m Unit
shouldEqual v1 v2 =
  when (v1 /= v2) $
    fail $ show v1 <> " ≠ " <> show v2

shouldNotEqual
  :: forall m t
   . MonadThrow Error m
  => Show t
  => Eq t
  => t
  -> t
  -> m Unit
shouldNotEqual v1 v2 =
  when (v1 == v2) $
    fail $ show v1 <> " = " <> show v2

shouldSatisfy
  :: forall m t
   . MonadThrow Error m
  => Show t
  => t
  -> (t -> Boolean)
  -> m Unit
shouldSatisfy v pred =
  unless (pred v) $
    fail $ show v <> " doesn't satisfy predicate"

shouldNotSatisfy
  :: forall m t
   . MonadThrow Error m
  => Show t
  => t
  -> (t -> Boolean)
  -> m Unit
shouldNotSatisfy v pred =
  when (pred v) $
    fail $ show v <> " satisfies predicate, but should not"

shouldContain
  :: forall m f a
   . MonadThrow Error m
  => Show a
  => Eq a
  => Show (f a)
  => Foldable f
  => f a
  -> a
  -> m Unit
shouldContain c e =
  when (e `notElem` c) $
    fail $ (show e) <> " ∉ " <> (show c)

shouldNotContain
  :: forall m f a
   . MonadThrow Error m
  => Show a
  => Eq a
  => Show (f a)
  => Foldable f
  => f a
  -> a
  -> m Unit
shouldNotContain c e =
  when (e `elem` c) $
    fail $ (show e) <> " ∈ " <> (show c)

expectError
  :: forall m t
   . MonadError Error m
  => m t
  -> m Unit
expectError a = do
  e <- try a
  case e of
    Left _ -> pure unit
    Right _ -> throwError $ error "Expected error"

-- | Asserts that `m t` returns `t`
shouldReturn
  :: forall m t
   . MonadThrow Error m
  => Eq t
  => Show t
  => m t
  -> t
  -> m Unit
shouldReturn ft t = ft >>= (_ `shouldEqual` t)

-- | Asserts that `m t` does not return `t`
shouldNotReturn
  :: forall m t
   . MonadThrow Error m
  => Eq t
  => Show t
  => m t
  -> t
  -> m Unit
shouldNotReturn ft t = ft >>= (_ `shouldNotEqual` t)
