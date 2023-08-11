module Data.Bifoldable where

import Prelude

import Control.Apply (applySecond)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Functor.Clown (Clown(..))
import Data.Functor.Flip (Flip(..))
import Data.Functor.Joker (Joker(..))
import Data.Functor.Product2 (Product2(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

-- | `Bifoldable` represents data structures with two type arguments which can be
-- | folded.
-- |
-- | A fold for such a structure requires two step functions, one for each type
-- | argument. Type class instances should choose the appropriate step function based
-- | on the type of the element encountered at each point of the fold.
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `bifoldrDefault`
-- | - `bifoldlDefault`
-- | - `bifoldMapDefaultR`
-- | - `bifoldMapDefaultL`
-- |
-- | Note: some combinations of the default implementations are unsafe to
-- | use together - causing a non-terminating mutually recursive cycle.
-- | These combinations are documented per function.
class Bifoldable p where
  bifoldr :: forall a b c. (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldl :: forall a b c. (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bifoldMap :: forall m a b. Monoid m => (a -> m) -> (b -> m) -> p a b -> m

instance bifoldableClown :: Foldable f => Bifoldable (Clown f) where
  bifoldr l _ u (Clown f) = foldr l u f
  bifoldl l _ u (Clown f) = foldl l u f
  bifoldMap l _ (Clown f) = foldMap l f

instance bifoldableJoker :: Foldable f => Bifoldable (Joker f) where
  bifoldr _ r u (Joker f) = foldr r u f
  bifoldl _ r u (Joker f) = foldl r u f
  bifoldMap _ r (Joker f) = foldMap r f

instance bifoldableFlip :: Bifoldable p => Bifoldable (Flip p) where
  bifoldr r l u (Flip p) = bifoldr l r u p
  bifoldl r l u (Flip p) = bifoldl l r u p
  bifoldMap r l (Flip p) = bifoldMap l r p

instance bifoldableProduct2 :: (Bifoldable f, Bifoldable g) => Bifoldable (Product2 f g) where
  bifoldr l r u m = bifoldrDefault l r u m
  bifoldl l r u m = bifoldlDefault l r u m
  bifoldMap l r (Product2 f g) = bifoldMap l r f <> bifoldMap l r g

instance bifoldableEither :: Bifoldable Either where
  bifoldr f _ z (Left a) = f a z
  bifoldr _ g z (Right b) = g b z
  bifoldl f _ z (Left a) = f z a
  bifoldl _ g z (Right b) = g z b
  bifoldMap f _ (Left a) = f a
  bifoldMap _ g (Right b) = g b

instance bifoldableTuple :: Bifoldable Tuple where
  bifoldMap f g (Tuple a b) = f a <> g b
  bifoldr f g z (Tuple a b) = f a (g b z)
  bifoldl f g z (Tuple a b) = g (f z a) b

instance bifoldableConst :: Bifoldable Const where
  bifoldr f _ z (Const a) = f a z
  bifoldl f _ z (Const a) = f z a
  bifoldMap f _ (Const a) = f a

-- | A default implementation of `bifoldr` using `bifoldMap`.
-- |
-- | Note: when defining a `Bifoldable` instance, this function is unsafe to
-- | use in combination with `bifoldMapDefaultR`.
bifoldrDefault
  :: forall p a b c
   . Bifoldable p
  => (a -> c -> c)
  -> (b -> c -> c)
  -> c
  -> p a b
  -> c
bifoldrDefault f g z p = unwrap (bifoldMap (Endo <<< f) (Endo <<< g) p) z

-- | A default implementation of `bifoldl` using `bifoldMap`.
-- |
-- | Note: when defining a `Bifoldable` instance, this function is unsafe to
-- | use in combination with `bifoldMapDefaultL`.
bifoldlDefault
  :: forall p a b c
   . Bifoldable p
  => (c -> a -> c)
  -> (c -> b -> c)
  -> c
  -> p a b
  -> c
bifoldlDefault f g z p =
  unwrap
    (unwrap
      (bifoldMap (Dual <<< Endo <<< flip f) (Dual <<< Endo <<< flip g) p))
    z

-- | A default implementation of `bifoldMap` using `bifoldr`.
-- |
-- | Note: when defining a `Bifoldable` instance, this function is unsafe to
-- | use in combination with `bifoldrDefault`.
bifoldMapDefaultR
  :: forall p m a b
   . Bifoldable p
  => Monoid m
  => (a -> m)
  -> (b -> m)
  -> p a b
  -> m
bifoldMapDefaultR f g = bifoldr (append <<< f) (append <<< g) mempty

-- | A default implementation of `bifoldMap` using `bifoldl`.
-- |
-- | Note: when defining a `Bifoldable` instance, this function is unsafe to
-- | use in combination with `bifoldlDefault`.
bifoldMapDefaultL
  :: forall p m a b
   . Bifoldable p
  => Monoid m
  => (a -> m)
  -> (b -> m)
  -> p a b
  -> m
bifoldMapDefaultL f g = bifoldl (\m a -> m <> f a) (\m b -> m <> g b) mempty


-- | Fold a data structure, accumulating values in a monoidal type.
bifold :: forall t m. Bifoldable t => Monoid m => t m m -> m
bifold = bifoldMap identity identity

-- | Traverse a data structure, accumulating effects using an `Applicative` functor,
-- | ignoring the final result.
bitraverse_
  :: forall t f a b c d
   . Bifoldable t
  => Applicative f
  => (a -> f c)
  -> (b -> f d)
  -> t a b
  -> f Unit
bitraverse_ f g = bifoldr (applySecond <<< f) (applySecond <<< g) (pure unit)

-- | A version of `bitraverse_` with the data structure as the first argument.
bifor_
  :: forall t f a b c d
   . Bifoldable t
  => Applicative f
  => t a b
  -> (a -> f c)
  -> (b -> f d)
  -> f Unit
bifor_ t f g = bitraverse_ f g t

-- | Collapse a data structure, collecting effects using an `Applicative` functor,
-- | ignoring the final result.
bisequence_
  :: forall t f a b
   . Bifoldable t
  => Applicative f
  => t (f a) (f b)
  -> f Unit
bisequence_ = bitraverse_ identity identity

-- | Test whether a predicate holds at any position in a data structure.
biany
  :: forall t a b c
   . Bifoldable t
  => BooleanAlgebra c
  => (a -> c)
  -> (b -> c)
  -> t a b
  -> c
biany p q = unwrap <<< bifoldMap (Disj <<< p) (Disj <<< q)

-- | Test whether a predicate holds at all positions in a data structure.
biall
  :: forall t a b c
   . Bifoldable t
  => BooleanAlgebra c
  => (a -> c)
  -> (b -> c)
  -> t a b
  -> c
biall p q = unwrap <<< bifoldMap (Conj <<< p) (Conj <<< q)
