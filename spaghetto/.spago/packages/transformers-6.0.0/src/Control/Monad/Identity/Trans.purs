module Control.Monad.Identity.Trans where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Effect.Class (class MonadEffect)

-- | The `IdentityT` monad transformer.
-- |
-- | This monad acts like a placeholder for functions that take a monad
-- | transformer as an argument, similar to `identity` for functions and
-- | `Identity` for monads.
newtype IdentityT :: forall k. (k -> Type) -> k -> Type
newtype IdentityT m a = IdentityT (m a)

-- | Run a computation in the `IdentityT` monad.
runIdentityT :: forall m a. IdentityT m a -> m a
runIdentityT (IdentityT ma) = ma

-- | Change the result type of a `IdentityT` monad action.
mapIdentityT :: forall m1 m2 a b. (m1 a -> m2 b) -> IdentityT m1 a -> IdentityT m2 b
mapIdentityT f (IdentityT m) = IdentityT (f m)

derive instance eqIdentityT :: (Eq1 m, Eq a) => Eq (IdentityT m a)
derive instance ordIdentityT :: (Ord1 m, Ord a) => Ord (IdentityT m a)
derive instance eq1IdentityT :: Eq1 m => Eq1 (IdentityT m)
derive instance ord1IdentityT :: Ord1 m => Ord1 (IdentityT m)
derive instance newtypeIdentityT :: Newtype (IdentityT m a) _

derive newtype instance functorIdentityT :: Functor m => Functor (IdentityT m)
derive newtype instance applyIdentityT :: Apply m => Apply (IdentityT m)
derive newtype instance applicativeIdentityT :: Applicative m => Applicative (IdentityT m)
derive newtype instance altIdentityT :: Alt m => Alt (IdentityT m)
derive newtype instance plusIdentityT :: Plus m => Plus (IdentityT m)
derive newtype instance alternativeIdentityT :: Alternative m => Alternative (IdentityT m)
derive newtype instance bindIdentityT :: Bind m => Bind (IdentityT m)
derive newtype instance monadIdentityT :: Monad m => Monad (IdentityT m)
derive newtype instance monadRecIdentityT :: MonadRec m => MonadRec (IdentityT m)
derive newtype instance monadPlusIdentityT :: MonadPlus m => MonadPlus (IdentityT m)

instance monadTransIdentityT :: MonadTrans IdentityT where
  lift = IdentityT

derive newtype instance monadEffectIdentityT :: MonadEffect m => MonadEffect (IdentityT m)
derive newtype instance monadContIdentityT :: MonadCont m => MonadCont (IdentityT m)
derive newtype instance monadThrowIdentityT :: MonadThrow e m => MonadThrow e (IdentityT m)
derive newtype instance monadErrorIdentityT :: MonadError e m => MonadError e (IdentityT m)
derive newtype instance monadAskIdentityT :: MonadAsk r m => MonadAsk r (IdentityT m)
derive newtype instance monadReaderIdentityT :: MonadReader r m => MonadReader r (IdentityT m)
derive newtype instance monadStateIdentityT :: MonadState s m => MonadState s (IdentityT m)
derive newtype instance monadTellIdentityT :: MonadTell w m => MonadTell w (IdentityT m)
derive newtype instance monadWriterIdentityT :: MonadWriter w m => MonadWriter w (IdentityT m)
derive newtype instance foldableIdentityT :: Foldable m => Foldable (IdentityT m)
derive newtype instance traversableIdentityT :: Traversable m => Traversable (IdentityT m)
