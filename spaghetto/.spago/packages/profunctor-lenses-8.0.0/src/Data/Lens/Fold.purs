-- | This module defines functions for working with folds.
module Data.Lens.Fold
  ( (^?)
  , previewOn
  , (^..)
  , toListOfOn
  , preview
  , foldOf
  , foldMapOf
  , foldrOf
  , foldlOf
  , toListOf
  , firstOf
  , lastOf
  , maximumOf
  , minimumOf
  , allOf
  , anyOf
  , andOf
  , orOf
  , elemOf
  , notElemOf
  , sumOf
  , productOf
  , lengthOf
  , findOf
  , sequenceOf_
  , traverseOf_
  , has
  , hasn't
  , replicated
  , filtered
  , folded
  , unfolded
  , toArrayOf
  , toArrayOfOn
  , ifoldMapOf
  , ifoldrOf
  , ifoldlOf
  , iallOf
  , ianyOf
  , ifindOf
  , itoListOf
  , itraverseOf_
  , iforOf_
  , module ExportTypes
  ) where

import Prelude

import Data.Array (fromFoldable) as Array
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldMap)
import Data.HeytingAlgebra (ff, tt)
import Data.Lens.Internal.Forget (Forget(..))
import Data.Lens.Types (Fold, Fold') as ExportTypes
import Data.Lens.Types (Fold, Indexed(..), IndexedFold, Optic')
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.First (First(..))
import Data.Maybe.Last (Last(..))
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (under, unwrap)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (class Choice, right)
import Data.Tuple (Tuple(..), uncurry)

-- | Previews the first value of a fold, if there is any.
preview :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
preview p = unwrap <<< foldMapOf p (First <<< Just)

-- | Synonym for `preview`, flipped.
previewOn :: forall s t a b. s -> Fold (First a) s t a b -> Maybe a
previewOn s p = preview p s

infixl 8 previewOn as ^?

-- | Folds all foci of a `Fold` to one. Note that this is the same as `view`.
foldOf :: forall s t a b. Fold a s t a b -> s -> a
foldOf p = foldMapOf p identity

-- | Maps and then folds all foci of a `Fold`.
foldMapOf :: forall s t a b r. Fold r s t a b -> (a -> r) -> s -> r
foldMapOf = under Forget

-- | Right fold over a `Fold`.
foldrOf :: forall s t a b r. Fold (Endo (->) r) s t a b -> (a -> r -> r) -> r -> s -> r
foldrOf p f r = flip unwrap r <<< foldMapOf p (Endo <<< f)

-- | Left fold over a `Fold`.
foldlOf :: forall s t a b r. Fold (Dual (Endo (->) r)) s t a b -> (r -> a -> r) -> r -> s -> r
foldlOf p f r = flip unwrap r <<< unwrap <<< foldMapOf p (Dual <<< Endo <<< flip f)

-- | Whether all foci of a `Fold` satisfy a predicate.
allOf :: forall s t a b r. HeytingAlgebra r => Fold (Conj r) s t a b -> (a -> r) -> s -> r
allOf p f = unwrap <<< foldMapOf p (Conj <<< f)

-- | Whether any focus of a `Fold` satisfies a predicate.
anyOf :: forall s t a b r. HeytingAlgebra r => Fold (Disj r) s t a b -> (a -> r) -> s -> r
anyOf p f = unwrap <<< foldMapOf p (Disj <<< f)

-- | The conjunction of all foci of a `Fold`.
andOf :: forall s t a b. HeytingAlgebra a => Fold (Conj a) s t a b -> s -> a
andOf p = allOf p identity

-- | The disjunction of all foci of a `Fold`.
orOf :: forall s t a b. HeytingAlgebra a => Fold (Disj a) s t a b -> s -> a
orOf p = anyOf p identity

-- | Whether a `Fold` contains a given element.
elemOf :: forall s t a b. Eq a => Fold (Disj Boolean) s t a b -> a -> s -> Boolean
elemOf p a = anyOf p (_ == a)

-- | Whether a `Fold` not contains a given element.
notElemOf :: forall s t a b. Eq a => Fold (Conj Boolean) s t a b -> a -> s -> Boolean
notElemOf p a = allOf p (_ /= a)

-- | The sum of all foci of a `Fold`.
sumOf :: forall s t a b. Semiring a => Fold (Additive a) s t a b -> s -> a
sumOf p = unwrap <<< foldMapOf p Additive

-- | The product of all foci of a `Fold`.
productOf :: forall s t a b. Semiring a => Fold (Multiplicative a) s t a b -> s -> a
productOf p = unwrap <<< foldMapOf p Multiplicative

-- | The number of foci of a `Fold`.
lengthOf :: forall s t a b. Fold (Additive Int) s t a b -> s -> Int
lengthOf p = unwrap <<< foldMapOf p (const $ Additive 1)

-- | The first focus of a `Fold`, if there is any. Synonym for `preview`.
firstOf :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
firstOf p = unwrap <<< foldMapOf p (First <<< Just)

-- | The last focus of a `Fold`, if there is any.
lastOf :: forall s t a b. Fold (Last a) s t a b -> s -> Maybe a
lastOf p = unwrap <<< foldMapOf p (Last <<< Just)

-- | The maximum of all foci of a `Fold`, if there is any.
maximumOf :: forall s t a b. Ord a => Fold (Endo (->) (Maybe a)) s t a b -> s -> Maybe a
maximumOf p = foldrOf p (\a -> Just <<< maybe a (max a)) Nothing
  where
  max a b = if a > b then a else b

-- | The minimum of all foci of a `Fold`, if there is any.
minimumOf :: forall s t a b. Ord a => Fold (Endo (->) (Maybe a)) s t a b -> s -> Maybe a
minimumOf p = foldrOf p (\a -> Just <<< maybe a (min a)) Nothing
  where
  min a b = if a < b then a else b

-- | Find the first focus of a `Fold` that satisfies a predicate, if there is any.
findOf :: forall s t a b. Fold (Endo (->) (Maybe a)) s t a b -> (a -> Boolean) -> s -> Maybe a
findOf p f = foldrOf p (\a -> maybe (if f a then Just a else Nothing) Just) Nothing

-- | Sequence the foci of a `Fold`, pulling out an `Applicative`, and ignore
-- | the result. If you need the result, see `sequenceOf` for `Traversal`s.
sequenceOf_
  :: forall f s t a b
   . Applicative f
  => Fold (Endo (->) (f Unit)) s t (f a) b
  -> s
  -> f Unit
sequenceOf_ p = flip unwrap (pure unit) <<< foldMapOf p \f -> Endo (f *> _)

-- | Traverse the foci of a `Fold`, discarding the results.
traverseOf_
  :: forall f s t a b r
   . Applicative f
  => Fold (Endo (->) (f Unit)) s t a b
  -> (a -> f r)
  -> s
  -> f Unit
traverseOf_ p f = foldrOf p (\a fu -> void (f a) *> fu) (pure unit)

-- | Collects the foci of a `Fold` into a list.
toListOf :: forall s t a b. Fold (Endo (->) (List a)) s t a b -> s -> List a
toListOf p = foldrOf p (:) Nil

-- | Synonym for `toListOf`, reversed.
toListOfOn :: forall s t a b. s -> Fold (Endo (->) (List a)) s t a b -> List a
toListOfOn s p = toListOf p s

infixl 8 toListOfOn as ^..

-- | Collects the foci of a `Fold` into an array.
toArrayOf :: forall s t a b. Fold (Endo (->) (List a)) s t a b -> s -> Array a
toArrayOf p = Array.fromFoldable <<< toListOf p

-- | Synonym for `toArrayOf`, reversed.
toArrayOfOn :: forall s t a b. s -> Fold (Endo (->) (List a)) s t a b -> Array a
toArrayOfOn s p = toArrayOf p s

-- | Determines whether a `Fold` has at least one focus.
has :: forall s t a b r. HeytingAlgebra r => Fold (Disj r) s t a b -> s -> r
has p = unwrap <<< foldMapOf p (const (Disj tt))

-- | Determines whether a `Fold` does not have a focus.
hasn't :: forall s t a b r. HeytingAlgebra r => Fold (Conj r) s t a b -> s -> r
hasn't p = unwrap <<< foldMapOf p (const (Conj ff))

-- | Filters on a predicate.
filtered :: forall p a. Choice p => (a -> Boolean) -> Optic' p a a
filtered f =
  right >>>
    dimap
      (\x -> if f x then Right x else Left x)
      (either identity identity)

-- | Replicates the elements of a fold.
replicated :: forall a b t r. Monoid r => Int -> Fold r a b a t
replicated i (Forget a) = Forget (go i a)
  where
  go 0 _ = mempty
  go n x = x <> go (n - 1) x

-- | Folds over a `Foldable` container.
folded :: forall g a b t r. Monoid r => Foldable g => Fold r (g a) b a t
folded (Forget a) = Forget (foldMap a)

-- | Builds a `Fold` using an unfold.
unfolded
  :: forall r s t a b
   . Monoid r
  => (s -> Maybe (Tuple a s))
  -> Fold r s t a b
unfolded f p = Forget go
  where
  go = maybe mempty (\(Tuple a sn) -> unwrap p a <> go sn) <<< f

-- | Fold map over an `IndexedFold`.
ifoldMapOf
  :: forall r i s t a b
   . IndexedFold r i s t a b
  -> (i -> a -> r)
  -> s
  -> r
ifoldMapOf p f = unwrap $ p $ Indexed $ Forget (uncurry f)

-- | Right fold over an `IndexedFold`.
ifoldrOf
  :: forall i s t a b r
   . IndexedFold (Endo (->) r) i s t a b
  -> (i -> a -> r -> r)
  -> r
  -> s
  -> r
ifoldrOf p f r = flip unwrap r <<< ifoldMapOf p (\i -> Endo <<< f i)

-- | Left fold over an `IndexedFold`.
ifoldlOf
  :: forall i s t a b r
   . IndexedFold (Dual (Endo (->) r)) i s t a b
  -> (i -> r -> a -> r)
  -> r
  -> s
  -> r
ifoldlOf p f r =
  flip unwrap r
    <<< unwrap
    <<< ifoldMapOf p (\i -> Dual <<< Endo <<< flip (f i))

-- | Whether all foci of an `IndexedFold` satisfy a predicate.
iallOf
  :: forall i s t a b r
   . HeytingAlgebra r
  => IndexedFold (Conj r) i s t a b
  -> (i -> a -> r)
  -> s
  -> r
iallOf p f = unwrap <<< ifoldMapOf p (\i -> Conj <<< f i)

-- | Whether any focus of an `IndexedFold` satisfies a predicate.
ianyOf
  :: forall i s t a b r
   . HeytingAlgebra r
  => IndexedFold (Disj r) i s t a b
  -> (i -> a -> r)
  -> s
  -> r
ianyOf p f = unwrap <<< ifoldMapOf p (\i -> Disj <<< f i)

-- | Find the first focus of an `IndexedFold` that satisfies a predicate, if
-- | there is any.
ifindOf
  :: forall i s t a b
   . IndexedFold (Endo (->) (Maybe a)) i s t a b
  -> (i -> a -> Boolean)
  -> s
  -> Maybe a
ifindOf p f =
  ifoldrOf
    p
    (\i a -> maybe (if f i a then Just a else Nothing) Just)
    Nothing

-- | Collects the foci of an `IndexedFold` into a list.
itoListOf
  :: forall i s t a b
   . IndexedFold (Endo (->) (List (Tuple i a))) i s t a b
  -> s
  -> List (Tuple i a)
itoListOf p = ifoldrOf p (\i x xs -> Tuple i x : xs) Nil

-- | Traverse the foci of an `IndexedFold`, discarding the results.
itraverseOf_
  :: forall i f s t a b r
   . (Applicative f)
  => IndexedFold (Endo (->) (f Unit)) i s t a b
  -> (i -> a -> f r)
  -> s
  -> f Unit
itraverseOf_ p f = ifoldrOf p (\i a fu -> void (f i a) *> fu) (pure unit)

-- | Flipped version of `itraverseOf_`.
iforOf_
  :: forall i f s t a b r
   . (Applicative f)
  => IndexedFold (Endo (->) (f Unit)) i s t a b
  -> s
  -> (i -> a -> f r)
  -> f Unit
iforOf_ = flip <<< itraverseOf_
