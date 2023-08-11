module Data.Coyoneda
  ( Coyoneda(..)
  , CoyonedaF
  , coyoneda
  , unCoyoneda
  , liftCoyoneda
  , lowerCoyoneda
  , hoistCoyoneda
  ) where

import Prelude

import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, (<<=))
import Control.Monad.Trans.Class (class MonadTrans)
import Control.MonadPlus (class MonadPlus)
import Data.Distributive (class Distributive, collect)
import Data.Eq (class Eq1, eq1)
import Data.Exists (Exists, runExists, mkExists)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Ord (class Ord1, compare1)
import Data.Semigroup.Foldable (class Foldable1, foldMap1, foldr1Default, foldl1Default)
import Data.Semigroup.Traversable (class Traversable1, sequence1, traverse1)
import Data.Traversable (class Traversable, traverse)

-- | `Coyoneda` is encoded as an existential type using `Data.Exists`.
-- |
-- | This type constructor encodes the contents of the existential package.
data CoyonedaF f a i = CoyonedaF (i -> a) (f i)

-- | The `Coyoneda` `Functor`.
-- |
-- | `Coyoneda f` is a `Functor` for any type constructor `f`. In fact,
-- | it is the _free_ `Functor` for `f`, i.e. any natural transformation
-- | `nat :: f ~> g`, can be factor through `liftCoyoneda`.  The natural
-- | transformation from `Coyoneda f ~> g` is given by `lowerCoyoneda <<<
-- | hoistCoyoneda nat`:
-- | ```purescript
-- | lowerCoyoneda <<< hoistCoyoneda nat <<< liftCoyoneda $ fi
-- | = lowerCoyoneda (hoistCoyoneda nat (Coyoneda $ mkExists $ CoyonedaF identity fi))    (by definition of liftCoyoneda)
-- | = lowerCoyoneda (coyoneda identity (nat fi))                                         (by definition of hoistCoyoneda)
-- | = unCoyoneda map (coyoneda identity (nat fi))                                        (by definition of lowerCoyoneda)
-- | = unCoyoneda map (Coyoneda $ mkExists $ CoyonedaF  identity (nat fi))                (by definition of coyoneda)
-- | = map identity (nat fi)                                                              (by definition of unCoyoneda)
-- | = nat fi                                                                       (since g is a Functor)
-- | ```
newtype Coyoneda f a = Coyoneda (Exists (CoyonedaF f a))

instance eqCoyoneda :: (Functor f, Eq1 f, Eq a) => Eq (Coyoneda f a) where
  eq x y = lowerCoyoneda x `eq1` lowerCoyoneda y

instance eq1Coyoneda :: (Functor f, Eq1 f) => Eq1 (Coyoneda f) where
  eq1 = eq

instance ordCoyoneda :: (Functor f, Ord1 f, Ord a) => Ord (Coyoneda f a) where
  compare x y = lowerCoyoneda x `compare1` lowerCoyoneda y

instance ord1Coyoneda :: (Functor f, Ord1 f) => Ord1 (Coyoneda f) where
  compare1 = compare

instance functorCoyoneda :: Functor (Coyoneda f) where
  map f (Coyoneda e) = runExists (\(CoyonedaF k fi) -> coyoneda (f <<< k) fi) e

instance invatiantCoyoneda :: Invariant (Coyoneda f) where
  imap = imapF

instance applyCoyoneda :: Apply f => Apply (Coyoneda f) where
  apply f g = liftCoyoneda $ lowerCoyoneda f <*> lowerCoyoneda g

instance applicativeCoyoneda :: Applicative f => Applicative (Coyoneda f) where
  pure = liftCoyoneda <<< pure

instance altCoyoneda :: Alt f => Alt (Coyoneda f) where
  alt x y = liftCoyoneda $ alt (lowerCoyoneda x) (lowerCoyoneda y)

instance plusCoyoneda :: Plus f => Plus (Coyoneda f) where
  empty = liftCoyoneda empty

instance alternativeCoyoneda :: Alternative f => Alternative (Coyoneda f)

instance bindCoyoneda :: Bind f => Bind (Coyoneda f) where
  bind (Coyoneda e) f =
    liftCoyoneda $
      runExists (\(CoyonedaF k fi) -> lowerCoyoneda <<< f <<< k =<< fi) e

-- | When `f` is a Monad then it is a functor as well.  In this case
-- | `liftCoyoneda` is not only a functor isomorphism but also a monad
-- | isomorphism, i.e. the following law holds
-- | ```purescript
-- | liftCoyoneda fa >>= liftCoyoneda <<< g = liftCoyoneda $ fa >>= g
-- | ```
instance monadCoyoneda :: Monad f => Monad (Coyoneda f)

instance monadTransCoyoneda :: MonadTrans Coyoneda where
  lift = liftCoyoneda

instance monadPlusCoyoneda :: MonadPlus f => MonadPlus (Coyoneda f)

instance extendCoyoneda :: Extend w => Extend (Coyoneda w) where
  extend f (Coyoneda e) =
    runExists (\(CoyonedaF k fi) -> liftCoyoneda $ f <<< coyoneda k <<= fi) e

-- | As in the monad case: if `w` is a comonad, then it is a functor, thus
-- | `liftCoyoneda` is an iso of functors, but moreover it is an iso of
-- | comonads, i.e. the following law holds:
-- | ```purescript
-- | g <<= liftCoyoneda w = liftCoyoneda $ g <<< liftCoyoneda <<= w
-- | ```
instance comonadCoyoneda :: Comonad w => Comonad (Coyoneda w) where
  extract (Coyoneda e) = runExists (\(CoyonedaF k fi) -> k $ extract fi) e

instance foldableCoyoneda :: Foldable f => Foldable (Coyoneda f) where
  foldr f z = unCoyoneda \k -> foldr (f <<< k) z
  foldl f z = unCoyoneda \k -> foldl (\x -> f x <<< k) z
  foldMap f = unCoyoneda \k -> foldMap (f <<< k)

instance traversableCoyoneda :: Traversable f => Traversable (Coyoneda f) where
  traverse f = unCoyoneda \k -> map liftCoyoneda <<< traverse (f <<< k)
  sequence = unCoyoneda \k -> map liftCoyoneda <<< traverse k

instance foldable1Coyoneda :: Foldable1 f => Foldable1 (Coyoneda f) where
  foldMap1 f = unCoyoneda \k -> foldMap1 (f <<< k)
  foldr1 = foldr1Default
  foldl1 = foldl1Default

instance traversable1Coyoneda :: Traversable1 f => Traversable1 (Coyoneda f) where
  traverse1 f = unCoyoneda \k -> map liftCoyoneda <<< traverse1 (f <<< k)
  sequence1 = unCoyoneda \k -> map liftCoyoneda <<< sequence1 <<< map k

instance distributiveCoyoneda :: Distributive f => Distributive (Coyoneda f) where
  collect f = liftCoyoneda <<< collect (lowerCoyoneda <<< f)
  distribute = liftCoyoneda <<< collect lowerCoyoneda

-- | Construct a value of type `Coyoneda f b` from a mapping function and a
-- | value of type `f a`.
coyoneda :: forall f a b. (a -> b) -> f a -> Coyoneda f b
coyoneda k fi = Coyoneda $ mkExists $ CoyonedaF k fi

-- | Deconstruct a value of `Coyoneda a` to retrieve the mapping function and
-- | original value.
unCoyoneda :: forall f a r. (forall b. (b -> a) -> f b -> r) -> Coyoneda f a -> r
unCoyoneda f (Coyoneda e) = runExists (\(CoyonedaF k fi) -> f k fi) e

-- | Lift a value described by the type constructor `f` to `Coyoneda f`.
-- |
-- | Note that for any functor `f` `liftCoyoneda` has a right inverse
-- | `lowerCoyoneda`:
-- | ```purescript
-- | liftCoyoneda <<< lowerCoyoneda $ (Coyoneda e)
-- | = liftCoyoneda <<< unCoyoneda map $ (Coyoneda e)
-- | = liftCoyonead (runExists (\(CoyonedaF k fi) -> map k fi) e)
-- | = liftCoyonead (Coyoneda e)
-- | = coyoneda identity (Coyoneda e)
-- | = Coyoneda e
-- | ```
-- | Moreover if `f` is a `Functor` then `liftCoyoneda` is an isomorphism of
-- | functors with inverse `lowerCoyoneda`:  we already showed that
-- | `lowerCoyoneda <<< hoistCoyoneda identity = lowerCoyoneda` is its left inverse
-- | whenever `f` is a functor.
liftCoyoneda :: forall f. f ~> Coyoneda f
liftCoyoneda = coyoneda identity

-- | Lower a value of type `Coyoneda f a` to the `Functor` `f`.
lowerCoyoneda :: forall f. Functor f => Coyoneda f ~> f
lowerCoyoneda = unCoyoneda map

-- | Use a natural transformation to change the generating type constructor of a
-- | `Coyoneda`.
hoistCoyoneda :: forall f g. (f ~> g) -> Coyoneda f ~> Coyoneda g
hoistCoyoneda nat (Coyoneda e) =
  runExists (\(CoyonedaF k fi) -> coyoneda k (nat fi)) e
