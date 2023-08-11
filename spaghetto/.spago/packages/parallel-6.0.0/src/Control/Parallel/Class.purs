module Control.Parallel.Class where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Reader.Trans (mapReaderT, ReaderT)
import Control.Monad.Writer.Trans (mapWriterT, WriterT)
import Control.Plus (class Plus)
import Data.Either (Either)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Costar (Costar(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor.Star (Star(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref

-- | The `Parallel` class abstracts over monads which support
-- | parallel composition via some related `Applicative`.
class (Monad m, Applicative f) <= Parallel f m | m -> f, f -> m where
  parallel :: m ~> f
  sequential :: f ~> m

instance monadParExceptT :: Parallel f m => Parallel (Compose f (Either e)) (ExceptT e m) where
  parallel (ExceptT ma) = Compose (parallel ma)
  sequential (Compose fa) = ExceptT (sequential fa)

instance monadParReaderT :: Parallel f m => Parallel (ReaderT e f) (ReaderT e m) where
  parallel = mapReaderT parallel
  sequential = mapReaderT sequential

instance monadParWriterT :: (Monoid w, Parallel f m) => Parallel (WriterT w f) (WriterT w m) where
  parallel = mapWriterT parallel
  sequential = mapWriterT sequential

instance monadParMaybeT :: Parallel f m => Parallel (Compose f Maybe) (MaybeT m) where
  parallel (MaybeT ma) = Compose (parallel ma)
  sequential (Compose fa) = MaybeT (sequential fa)

instance monadParStar :: Parallel f m => Parallel (Star f a) (Star m a) where
  parallel (Star f) = (Star $ parallel <<< f)
  sequential (Star f) = (Star $ sequential <<< f)

instance monadParCostar :: Parallel f m => Parallel (Costar f a) (Costar m a) where
  parallel (Costar f) = (Costar $ sequential >>> f)
  sequential (Costar f) = (Costar $ parallel >>> f)


-- | The `ParCont` type constructor provides an `Applicative` instance
-- | based on `ContT Unit m`, which waits for multiple continuations to be
-- | resumed simultaneously.
-- |
-- | ParCont sections of code can be embedded in sequential code by using
-- | the `parallel` and `sequential` functions:
-- |
-- | ```purescript
-- | loadModel :: ContT Unit (Eff (ajax :: AJAX)) Model
-- | loadModel = do
-- |   token <- authenticate
-- |   sequential $
-- |     Model <$> parallel (get "/products/popular/" token)
-- |           <*> parallel (get "/categories/all" token)
-- | ```
newtype ParCont m a = ParCont (ContT Unit m a)

derive instance newtypeParCont :: Newtype (ParCont m a) _

instance functorParCont :: MonadEffect m => Functor (ParCont m) where
  map f = parallel <<< map f <<< sequential

instance applyParCont :: MonadEffect m => Apply (ParCont m) where
  apply (ParCont ca) (ParCont cb) = ParCont $ ContT \k -> do
    ra <- liftEffect (Ref.new Nothing)
    rb <- liftEffect (Ref.new Nothing)

    runContT ca \a -> do
      mb <- liftEffect (Ref.read rb)
      case mb of
        Nothing -> liftEffect (Ref.write (Just a) ra)
        Just b -> k (a b)

    runContT cb \b -> do
      ma <- liftEffect (Ref.read ra)
      case ma of
        Nothing -> liftEffect (Ref.write (Just b) rb)
        Just a -> k (a b)

instance applicativeParCont :: MonadEffect m => Applicative (ParCont m) where
  pure = parallel <<< pure

instance altParCont :: MonadEffect m => Alt (ParCont m) where
  alt (ParCont c1) (ParCont c2) = ParCont $ ContT \k -> do
    done <- liftEffect (Ref.new false)

    runContT c1 \a -> do
      b <- liftEffect (Ref.read done)
      if b
        then pure unit
        else do
          liftEffect (Ref.write true done)
          k a

    runContT c2 \a -> do
      b <- liftEffect (Ref.read done)
      if b
        then pure unit
        else do
          liftEffect (Ref.write true done)
          k a

instance plusParCont :: MonadEffect m => Plus (ParCont m) where
  empty = ParCont $ ContT \_ -> pure unit

instance alternativeParCont :: MonadEffect m => Alternative (ParCont m)

instance monadParParCont :: MonadEffect m => Parallel (ParCont m) (ContT Unit m) where
  parallel = ParCont
  sequential (ParCont ma) = ma
