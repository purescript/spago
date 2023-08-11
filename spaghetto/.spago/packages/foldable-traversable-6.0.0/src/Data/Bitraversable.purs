module Data.Bitraversable
  ( class Bitraversable, bitraverse, bisequence
  , bitraverseDefault
  , bisequenceDefault
  , ltraverse
  , rtraverse
  , bifor
  , lfor
  , rfor
  , module Data.Bifoldable
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable, biall, biany, bifold, bifoldMap, bifoldMapDefaultL, bifoldMapDefaultR, bifoldl, bifoldlDefault, bifoldr, bifoldrDefault, bifor_, bisequence_, bitraverse_)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Functor.Clown (Clown(..))
import Data.Functor.Flip (Flip(..))
import Data.Functor.Joker (Joker(..))
import Data.Functor.Product2 (Product2(..))
import Data.Tuple (Tuple(..))

-- | `Bitraversable` represents data structures with two type arguments which can be
-- | traversed.
-- |
-- | A traversal for such a structure requires two functions, one for each type
-- | argument. Type class instances should choose the appropriate function based
-- | on the type of the element encountered at each point of the traversal.
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `bitraverseDefault`
-- | - `bisequenceDefault`
class (Bifunctor t, Bifoldable t) <= Bitraversable t where
  bitraverse :: forall f a b c d. Applicative f => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bisequence :: forall f a b. Applicative f => t (f a) (f b) -> f (t a b)

instance bitraversableClown :: Traversable f => Bitraversable (Clown f) where
  bitraverse l _ (Clown f) = Clown <$> traverse l f
  bisequence (Clown f) = Clown <$> sequence f

instance bitraversableJoker :: Traversable f => Bitraversable (Joker f) where
  bitraverse _ r (Joker f) = Joker <$> traverse r f
  bisequence (Joker f) = Joker <$> sequence f

instance bitraversableFlip :: Bitraversable p => Bitraversable (Flip p) where
  bitraverse r l (Flip p) = Flip <$> bitraverse l r p
  bisequence (Flip p) = Flip <$> bisequence p

instance bitraversableProduct2 :: (Bitraversable f, Bitraversable g) => Bitraversable (Product2 f g) where
  bitraverse l r (Product2 f g) = Product2 <$> bitraverse l r f <*> bitraverse l r g
  bisequence (Product2 f g) = Product2 <$> bisequence f <*> bisequence g

instance bitraversableEither :: Bitraversable Either where
  bitraverse f _ (Left a) = Left <$> f a
  bitraverse _ g (Right b) = Right <$> g b
  bisequence (Left a) = Left <$> a
  bisequence (Right b) = Right <$> b

instance bitraversableTuple :: Bitraversable Tuple where
  bitraverse f g (Tuple a b) = Tuple <$> f a <*> g b
  bisequence (Tuple a b) = Tuple <$> a <*> b

instance bitraversableConst :: Bitraversable Const where
  bitraverse f _ (Const a) = Const <$> f a
  bisequence (Const a) = Const <$> a

ltraverse
  :: forall t b c a f
   . Bitraversable t
  => Applicative f
  => (a -> f c)
  -> t a b
  -> f (t c b)
ltraverse f = bitraverse f pure

rtraverse
  :: forall t b c a f
   . Bitraversable t
  => Applicative f
  => (b -> f c)
  -> t a b
  -> f (t a c)
rtraverse = bitraverse pure

-- | A default implementation of `bitraverse` using `bisequence` and `bimap`.
bitraverseDefault
  :: forall t f a b c d
   . Bitraversable t
  => Applicative f
  => (a -> f c)
  -> (b -> f d)
  -> t a b
  -> f (t c d)
bitraverseDefault f g t = bisequence (bimap f g t)

-- | A default implementation of `bisequence` using `bitraverse`.
bisequenceDefault
  :: forall t f a b
   . Bitraversable t
  => Applicative f
  => t (f a) (f b)
  -> f (t a b)
bisequenceDefault = bitraverse identity identity

-- | Traverse a data structure, accumulating effects and results using an `Applicative` functor.
bifor
  :: forall t f a b c d
   . Bitraversable t
  => Applicative f
  => t a b
  -> (a -> f c)
  -> (b -> f d)
  -> f (t c d)
bifor t f g = bitraverse f g t

lfor
  :: forall t b c a f
   . Bitraversable t
  => Applicative f
  => t a b
  -> (a -> f c)
  -> f (t c b)
lfor t f = bitraverse f pure t

rfor
  :: forall t b c a f
   . Bitraversable t
  => Applicative f
  => t a b
  -> (b -> f c)
  -> f (t a c)
rfor t f = bitraverse pure f t
