-- | A port of Haskell's [mmorph library](http://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html)
module Control.Monad.Morph where

import Prelude

import Control.Comonad.Cofree (Cofree, hoistCofree)
import Control.Comonad.Env.Trans as Env
import Control.Comonad.Store as Store
import Control.Comonad.Traced as T
import Control.Monad.Except.Trans as E
import Control.Monad.Free (Free, foldFree, hoistFree)
import Control.Monad.Maybe.Trans as M
import Control.Monad.RWS.Trans as RWS
import Control.Monad.Reader.Trans as R
import Control.Monad.State.Trans as S
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Trans as W
import Data.Bifunctor (lmap)
import Data.Coyoneda (Coyoneda, hoistCoyoneda)
import Data.Either (Either(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Product (Product(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Tuple (Tuple(..))
import Data.Yoneda (Yoneda, hoistYoneda, lowerYoneda)

class MFunctor (t :: (Type -> Type) -> Type -> Type) where
  hoist :: forall m n. Monad m => m ~> n -> t m ~> t n

instance mfunctorExceptT :: MFunctor (E.ExceptT e) where
  hoist nat m = E.ExceptT (nat (E.runExceptT m))

instance mfunctorMaybe :: MFunctor M.MaybeT where
  hoist nat m = M.MaybeT (nat (M.runMaybeT m))

instance mfunctorReaderT :: MFunctor (R.ReaderT r) where
  hoist nat m = R.ReaderT (\ i -> nat (R.runReaderT m i))

instance mfunctorWriterT :: MFunctor (W.WriterT w) where
  hoist nat m = W.WriterT (nat (W.runWriterT m))

instance mfunctorStateT :: MFunctor (S.StateT s) where
  hoist nat m = S.StateT (\ s -> nat (S.runStateT m s))

instance mfunctorRWS :: MFunctor (RWS.RWST r w s) where
  hoist nat m = RWS.RWST (\ r s -> nat (RWS.runRWST m r s))

instance mfunctorCompose :: (Functor f) => MFunctor (Compose f) where
  hoist nat (Compose f) = Compose (map nat f)

instance mfunctorProduct :: MFunctor (Product f) where
  hoist nat (Product (Tuple f g)) = Product (Tuple f (nat g))

instance mfunctorYoneda :: MFunctor Yoneda where
  hoist = hoistYoneda

instance mfunctorCoyoneda :: MFunctor Coyoneda where
  hoist = hoistCoyoneda

instance mfunctorFree :: MFunctor Free where
  hoist = hoistFree

instance mfunctorCofree :: MFunctor Cofree where
  hoist = hoistCofree

instance mfunctorEnvT :: MFunctor (Env.EnvT e) where
  hoist nat = over Env.EnvT (map nat)

instance mfunctorTracedT :: MFunctor (T.TracedT t) where
  hoist nat = over T.TracedT nat

instance mfunctorStoreT :: MFunctor (Store.StoreT s) where
  hoist nat = over Store.StoreT (lmap nat)

generalize :: forall m a. Monad m => Identity a -> m a
generalize = pure <<< unwrap

class (MFunctor t, MonadTrans t) <= MMonad t where
  embed :: forall n m b. Monad n => (forall a. m a -> t n a) -> t m b -> t n b

squash :: forall m t. Monad m => MMonad t => t (t m) ~> t m
squash = embed identity

infixr 2 composeKleisliRight as >|>
infixl 2 composeKleisliLeft as <|<

infixr 2 embed as =<|
infixl 2 flipEmbed as |>=

composeKleisliRight
  :: forall m1 m2 m3 t
   . MMonad t
  => Monad m3
  => m1 ~> t m2
  -> m2 ~> t m3
  -> m1 ~> t m3
composeKleisliRight f g m = embed g (f m)

composeKleisliLeft
  :: forall m1 m2 m3 t
   . MMonad t
  => Monad m3
  => m2 ~> t m3
  -> m1 ~> t m2
  -> m1 ~> t m3
composeKleisliLeft g f m = embed g (f m)

flipEmbed
  :: forall t m n a
   . MMonad t
  => Monad n
  => t m a
  -> m ~> t n
  -> t n a
flipEmbed t f = embed f t

instance mmonadExceptT :: MMonad (E.ExceptT e) where
  embed f m = E.ExceptT do
    x <- E.runExceptT (f (E.runExceptT m))
    pure case x of
      Left e -> Left e
      Right (Left e) -> Left e
      Right (Right a) -> Right a

instance mmonadMaybeT :: MMonad M.MaybeT where
  embed f m = M.MaybeT do
    x <- M.runMaybeT (f (M.runMaybeT m))
    pure case x of
      Nothing -> Nothing
      Just Nothing -> Nothing
      Just (Just a) -> Just a

instance mmonadReaderT :: MMonad (R.ReaderT r) where
  embed f m = R.ReaderT \i -> R.runReaderT (f (R.runReaderT m i)) i

instance mmonadWriterT :: (Monoid w) => MMonad (W.WriterT w) where
  embed f m = W.WriterT do
    Tuple (Tuple a w1) w2 <- W.runWriterT (f (W.runWriterT m))
    pure (Tuple a (append w1 w2))

instance mmonadFree :: MMonad Free where
  embed = foldFree

instance mmonadYoneda :: MMonad Yoneda where
  embed f = f <$> lowerYoneda
