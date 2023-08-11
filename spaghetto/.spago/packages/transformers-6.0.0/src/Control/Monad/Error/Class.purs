-- | This module defines the `MonadError` type class and its instances.

module Control.Monad.Error.Class where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Exception as Ex


-- | The `MonadThrow` type class represents those monads which support errors via
-- | `throwError`, where `throwError e` halts, yielding the error `e`.
-- |
-- | An implementation is provided for `ExceptT`, and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - Left zero: `throwError e >>= f = throwError e`
-- |
class Monad m <= MonadThrow e m | m -> e where
  throwError :: forall a. e -> m a

-- | The `MonadError` type class represents those monads which support catching
-- | errors.
-- |
-- | - `catchError x f` calls the error handler `f` if an error is thrown during the
-- |   evaluation of `x`.
-- |
-- | An implementation is provided for `ExceptT`, and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - Catch: `catchError (throwError e) f = f e`
-- | - Pure: `catchError (pure a) f = pure a`
-- |
class MonadThrow e m <= MonadError e m | m -> e where
  catchError :: forall a. m a -> (e -> m a) -> m a

-- | This function allows you to provide a predicate for selecting the
-- | exceptions that you're interested in, and handle only those exceptons.
-- | If the inner computation throws an exception, and the predicate returns
-- | Nothing, then the whole computation will still fail with that exception.
catchJust
  :: forall e m a b
   . MonadError e m
  => (e -> Maybe b) -- ^ Predicate to select exceptions
  -> m a            -- ^ Computation to run
  -> (b -> m a)     -- ^ Handler
  -> m a
catchJust p act handler = catchError act handle
  where
  handle e =
    case p e of
      Nothing -> throwError e
      Just b -> handler b

-- | Return `Right` if the given action succeeds, `Left` if it throws.
try
  :: forall e m a
   . MonadError e m
  => m a
  -> m (Either e a)
try a = (Right <$> a) `catchError` (pure <<< Left)

instance monadThrowEither :: MonadThrow e (Either e) where
  throwError = Left

instance monadErrorEither :: MonadError e (Either e) where
  catchError (Left e) h = h e
  catchError (Right x) _ = Right x

instance monadThrowMaybe :: MonadThrow Unit Maybe where
  throwError = const Nothing

instance monadErrorMaybe :: MonadError Unit Maybe where
  catchError Nothing f  = f unit
  catchError (Just a) _ = Just a
 
instance monadThrowEffect :: MonadThrow Ex.Error Effect where
  throwError = Ex.throwException

instance monadErrorEffect :: MonadError Ex.Error Effect where
  catchError = flip Ex.catchException


-- | Make sure that a resource is cleaned up in the event of an exception. The
-- | release action is called regardless of whether the body action throws or
-- | returns.
withResource
  :: forall e m r a
   . MonadError e m
  => m r
  -> (r -> m Unit)
  -> (r -> m a)
  -> m a
withResource acquire release kleisli = do
  resource <- acquire
  result <- try $ kleisli resource
  release resource
  either throwError pure result

-- | Lift a `Maybe` value to a MonadThrow monad.
liftMaybe :: forall m e a. MonadThrow e m => e -> Maybe a -> m a
liftMaybe error = maybe (throwError error) pure

-- | Lift an `Either` value to a MonadThrow monad.
liftEither :: forall m e a. MonadThrow e m => Either e a -> m a
liftEither = either throwError pure
