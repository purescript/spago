module Control.Monad.Rec.Class
  ( Step(..)
  , class MonadRec
  , tailRec
  , tailRec2
  , tailRec3
  , tailRecM
  , tailRecM2
  , tailRecM3
  , forever
  , whileJust
  , untilJust
  , loop2
  , loop3
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Effect (Effect, untilE)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)

-- | The result of a computation: either `Loop` containing the updated
-- | accumulator, or `Done` containing the final result of the computation.
data Step a b = Loop a | Done b

derive instance functorStep :: Functor (Step a)

instance bifunctorStep :: Bifunctor Step where
  bimap f _ (Loop a) = Loop (f a)
  bimap _ g (Done b) = Done (g b)

-- | This type class captures those monads which support tail recursion in
-- | constant stack space.
-- |
-- | The `tailRecM` function takes a step function, and applies that step
-- | function recursively until a pure value of type `b` is found.
-- |
-- | Instances are provided for standard monad transformers.
-- |
-- | For example:
-- |
-- | ```purescript
-- | loopWriter :: Int -> WriterT (Additive Int) Effect Unit
-- | loopWriter n = tailRecM go n
-- |   where
-- |   go 0 = do
-- |     traceM "Done!"
-- |     pure (Done unit)
-- |   go i = do
-- |     tell $ Additive i
-- |     pure (Loop (i - 1))
-- | ```
class Monad m <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Step a b)) -> a -> m b

-- | Create a tail-recursive function of two arguments which uses constant stack space.
-- |
-- | The `loop2` helper function provides a curried alternative to the `Loop`
-- | constructor for this function.
tailRecM2
  :: forall m a b c
   . MonadRec m
  => (a -> b -> m (Step { a :: a, b :: b } c))
  -> a
  -> b
  -> m c
tailRecM2 f a b = tailRecM (\o -> f o.a o.b) { a, b }

-- | Create a tail-recursive function of three arguments which uses constant stack space.
-- |
-- | The `loop3` helper function provides a curried alternative to the `Loop`
-- | constructor for this function.
tailRecM3
  :: forall m a b c d
   . MonadRec m
  => (a -> b -> c -> m (Step { a :: a, b :: b, c :: c } d))
  -> a
  -> b
  -> c
  -> m d
tailRecM3 f a b c = tailRecM (\o -> f o.a o.b o.c) { a, b, c }

-- | Create a pure tail-recursive function of one argument
-- |
-- | For example:
-- |
-- | ```purescript
-- | pow :: Int -> Int -> Int
-- | pow n p = tailRec go { accum: 1, power: p }
-- |   where
-- |   go :: _ -> Step _ Int
-- |   go { accum: acc, power: 0 } = Done acc
-- |   go { accum: acc, power: p } = Loop { accum: acc * n, power: p - 1 }
-- | ```
tailRec :: forall a b. (a -> Step a b) -> a -> b
tailRec f = go <<< f
  where
  go (Loop a) = go (f a)
  go (Done b) = b

-- | Create a pure tail-recursive function of two arguments
-- |
-- | The `loop2` helper function provides a curried alternative to the `Loop`
-- | constructor for this function.
tailRec2 :: forall a b c. (a -> b -> Step { a :: a, b :: b } c) -> a -> b -> c
tailRec2 f a b = tailRec (\o -> f o.a o.b) { a, b }

-- | Create a pure tail-recursive function of three arguments
-- |
-- | The `loop3` helper function provides a curried alternative to the `Loop`
-- | constructor for this function.
tailRec3 :: forall a b c d. (a -> b -> c -> Step { a :: a, b :: b, c :: c } d) -> a -> b -> c -> d
tailRec3 f a b c = tailRec (\o -> f o.a o.b o.c) { a, b, c }

instance monadRecIdentity :: MonadRec Identity where
  tailRecM f = Identity <<< tailRec (runIdentity <<< f)
    where runIdentity (Identity x) = x

instance monadRecEffect :: MonadRec Effect where
  tailRecM f a = do
    r <- Ref.new =<< f a
    untilE do
      Ref.read r >>= case _ of
        Loop a' -> do
          e <- f a'
          _ <- Ref.write e r
          pure false
        Done _ -> pure true
    fromDone <$> Ref.read r
    where
    fromDone :: forall a b. Step a b -> b
    fromDone = unsafePartial \(Done b) -> b

instance monadRecFunction :: MonadRec ((->) e) where
  tailRecM f a0 e = tailRec (\a -> f a e) a0

instance monadRecEither :: MonadRec (Either e) where
  tailRecM f a0 =
    let
      g (Left e) = Done (Left e)
      g (Right (Loop a)) = Loop (f a)
      g (Right (Done b)) = Done (Right b)
    in tailRec g (f a0)

instance monadRecMaybe :: MonadRec Maybe where
  tailRecM f a0 =
    let
      g Nothing = Done Nothing
      g (Just (Loop a)) = Loop (f a)
      g (Just (Done b)) = Done (Just b)
    in tailRec g (f a0)

-- | `forever` runs an action indefinitely, using the `MonadRec` instance to
-- | ensure constant stack usage.
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = forever $ trace "Hello, World!"
-- | ```
forever :: forall m a b. MonadRec m => m a -> m b
forever ma = tailRecM (\u -> Loop u <$ ma) unit

-- | While supplied computation evaluates to `Just _`, it will be
-- | executed repeatedly and results will be combined using monoid instance.
whileJust :: forall a m. Monoid a => MonadRec m => m (Maybe a) -> m a
whileJust m = mempty # tailRecM \v -> m <#> case _ of
  Nothing -> Done v
  Just x -> Loop $ v <> x

-- | Supplied computation will be executed repeatedly until it evaluates
-- | to `Just value` and then that `value` will be returned.
untilJust :: forall a m. MonadRec m => m (Maybe a) -> m a
untilJust m = unit # tailRecM \_ -> m <#> case _ of
  Nothing -> Loop unit
  Just x -> Done x

-- | A curried version of the `Loop` constructor, provided as a convenience for
-- | use with `tailRec2` and `tailRecM2`.
loop2 :: forall a b c. a -> b -> Step { a :: a, b :: b } c
loop2 a b = Loop { a, b }

-- | A curried version of the `Loop` constructor, provided as a convenience for
-- | use with `tailRec3` and `tailRecM3`.
loop3 :: forall a b c d. a -> b -> c -> Step { a :: a, b :: b, c :: c } d
loop3 a b c = Loop { a, b, c }
