module Data.Lens.Internal.Bazaar where

import Prelude

import Data.Bitraversable (bitraverse)
import Data.Lens.Internal.Wander (class Wander)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- | This is used to characterize a Traversal.
newtype Bazaar :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Type
newtype Bazaar p a b s t = Bazaar (forall f. Applicative f => p a (f b) -> s -> f t)

runBazaar :: forall p a b s t. Bazaar p a b s t -> (forall f. Applicative f => p a (f b) -> s -> f t)
runBazaar (Bazaar x) = x

instance profunctorBazaar :: Profunctor (Bazaar p a b) where
  dimap f g (Bazaar b) = Bazaar \pafb s -> g <$> b pafb (f s)

instance strongBazaar :: Strong (Bazaar p a b) where
  first (Bazaar b) = Bazaar (\pafb (Tuple x y) -> flip Tuple y <$> b pafb x)
  second (Bazaar b) = Bazaar (\pafb (Tuple x y) -> Tuple x <$> b pafb y)

instance choiceBazaar :: Choice (Bazaar p a b) where
  left (Bazaar b) = Bazaar (\pafb e -> bitraverse (b pafb) pure e)
  right (Bazaar b) = Bazaar (\pafb e -> traverse (b pafb) e)

instance wanderBazaar :: Wander (Bazaar p a b) where
  wander w (Bazaar f) = Bazaar (\pafb s -> w (f pafb) s)
