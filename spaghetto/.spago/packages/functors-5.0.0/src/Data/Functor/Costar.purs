module Data.Functor.Costar where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, (=<=))
import Data.Bifunctor (class Bifunctor)
import Data.Distributive (class Distributive, distribute)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Closed (class Closed)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)

-- | `Costar` turns a `Functor` into a `Profunctor` "backwards".
-- |
-- | `Costar f` is also the co-Kleisli category for `f`.
newtype Costar :: (Type -> Type) -> Type -> Type -> Type
newtype Costar f b a = Costar (f b -> a)

derive instance newtypeCostar :: Newtype (Costar f a b) _

instance semigroupoidCostar :: Extend f => Semigroupoid (Costar f) where
  compose (Costar f) (Costar g) = Costar (f =<= g)

instance categoryCostar :: Comonad f => Category (Costar f) where
  identity = Costar extract

instance functorCostar :: Functor (Costar f a) where
  map f (Costar g) = Costar (f <<< g)

instance invariantCostar :: Invariant (Costar f a) where
  imap = imapF

instance applyCostar :: Apply (Costar f a) where
  apply (Costar f) (Costar g) = Costar \a -> f a (g a)

instance applicativeCostar :: Applicative (Costar f a) where
  pure a = Costar \_ -> a

instance bindCostar :: Bind (Costar f a) where
  bind (Costar m) f = Costar \x -> case f (m x) of Costar g -> g x

instance monadCostar :: Monad (Costar f a)

instance distributiveCostar :: Distributive (Costar f a) where
  distribute f = Costar \a -> map (\(Costar g) -> g a) f
  collect f = distribute <<< map f

instance bifunctorCostar :: Contravariant f => Bifunctor (Costar f) where
  bimap f g (Costar h) = Costar (cmap f >>> h >>> g)

instance profunctorCostar :: Functor f => Profunctor (Costar f) where
  dimap f g (Costar h) = Costar (map f >>> h >>> g)

instance strongCostar :: Comonad f => Strong (Costar f) where
  first (Costar f) = Costar \x -> Tuple (f (map fst x)) (snd (extract x))
  second (Costar f) = Costar \x -> Tuple (fst (extract x)) (f (map snd x))

instance closedCostar :: Functor f => Closed (Costar f) where
  closed (Costar f) = Costar \g x -> f (map (_ $ x) g)

hoistCostar :: forall f g a b. (g ~> f) -> Costar f a b -> Costar g a b
hoistCostar f (Costar g) = Costar (lcmap f g)
