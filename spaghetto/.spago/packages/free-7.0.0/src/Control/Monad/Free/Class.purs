module Control.Monad.Free.Class
  ( class MonadFree
  , wrapFree
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Control.Monad.State.Trans (StateT(..), runStateT)
import Control.Monad.Writer.Trans (WriterT(..), runWriterT)

-- | Based on <http://hackage.haskell.org/package/free/docs/Control-Monad-Free-Class.html>
class Monad m <= MonadFree f m | m -> f where
  wrapFree :: forall a. f (m a) -> m a

instance monadFreeFree :: MonadFree f (Free f) where
  wrapFree = join <<< liftF

instance monadFreeReaderT :: (Functor f, MonadFree f m) => MonadFree f (ReaderT r m) where
  wrapFree f = ReaderT \r -> wrapFree (map (\rt -> runReaderT rt r) f)

instance monadFreeStateT :: (Functor f, MonadFree f m) => MonadFree f (StateT s m) where
  wrapFree f = StateT \s -> wrapFree (map (\st -> runStateT st s) f)

instance monadFreeWriterT :: (Functor f, MonadFree f m, Monoid w) => MonadFree f (WriterT w m) where
  wrapFree f = WriterT (wrapFree (map runWriterT f))

instance monadFreeMaybeT :: (Functor f, MonadFree f m) => MonadFree f (MaybeT m) where
  wrapFree f = MaybeT (wrapFree (map runMaybeT f))

instance monadFreeExceptT :: (Functor f, MonadFree f m) => MonadFree f (ExceptT e m) where
  wrapFree f = ExceptT (wrapFree (map runExceptT f))
