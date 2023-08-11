module Data.List.Lazy.Types where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Control.Lazy as Z
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex, foldrWithIndex, foldMapWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), snd)
import Data.Unfoldable (class Unfoldable, unfoldr1)
import Data.Unfoldable1 (class Unfoldable1)

-- | A lazy linked list.
newtype List a = List (Lazy (Step a))

-- | A list is either empty (represented by the `Nil` constructor) or non-empty, in
-- | which case it consists of a head element, and another list (represented by the
-- | `Cons` constructor).
data Step a = Nil | Cons a (List a)

instance showStep :: Show a => Show (Step a) where
  show Nil = "Nil"
  show (Cons x xs) = "(" <> show x <> " : " <> show xs <> ")"

-- | Unwrap a lazy linked list
step :: forall a. List a -> Step a
step = force <<< unwrap

-- | The empty list.
-- |
-- | Running time: `O(1)`
nil :: forall a. List a
nil = List $ defer \_ -> Nil

-- | Attach an element to the front of a lazy list.
-- |
-- | Running time: `O(1)`
cons :: forall a. a -> List a -> List a
cons x xs = List $ defer \_ -> Cons x xs

-- | An infix alias for `cons`; attaches an element to the front of
-- | a list.
-- |
-- | Running time: `O(1)`
infixr 6 cons as :

derive instance newtypeList :: Newtype (List a) _

instance showList :: Show a => Show (List a) where
  show xs = "(fromFoldable ["
         <> case step xs of
              Nil -> ""
              Cons x xs' ->
                show x <> foldl (\shown x' -> shown <> "," <> show x') "" xs'
         <> "])"

instance eqList :: Eq a => Eq (List a) where
  eq = eq1

instance eq1List :: Eq1 List where
  eq1 xs ys = go (step xs) (step ys)
    where
    go Nil Nil = true
    go (Cons x xs') (Cons y ys')
      | x == y = go (step xs') (step ys')
    go _ _ = false

instance ordList :: Ord a => Ord (List a) where
  compare = compare1

instance ord1List :: Ord1 List where
  compare1 xs ys = go (step xs) (step ys)
    where
    go Nil Nil = EQ
    go Nil _   = LT
    go _   Nil = GT
    go (Cons x xs') (Cons y ys') =
      case compare x y of
        EQ -> go (step xs') (step ys')
        other -> other

instance lazyList :: Z.Lazy (List a) where
  defer f = List $ defer (step <<< f)

instance semigroupList :: Semigroup (List a) where
  append xs ys = List (go <$> unwrap xs)
    where
    go Nil = step ys
    go (Cons x xs') = Cons x (xs' <> ys)

instance monoidList :: Monoid (List a) where
  mempty = nil

instance functorList :: Functor List where
  map f xs = List (go <$> unwrap xs)
    where
    go Nil = Nil
    go (Cons x xs') = Cons (f x) (f <$> xs')

instance functorWithIndexList :: FunctorWithIndex Int List where
  mapWithIndex f = foldrWithIndex (\i x acc -> f i x : acc) nil

instance foldableList :: Foldable List where
  -- calls foldl on the reversed list
  foldr op z xs = foldl (flip op) z (rev xs) where
    rev = foldl (flip cons) nil

  foldl op = go
    where
    -- `go` is needed to ensure the function is tail-call optimized
    go b xs =
      case step xs of
        Nil -> b
        Cons hd tl -> go (b `op` hd) tl

  foldMap f = foldl (\b a -> b <> f a) mempty

instance foldableWithIndexList :: FoldableWithIndex Int List where
  foldrWithIndex f b xs =
    -- as we climb the reversed list, we decrement the index
    snd $ foldl
            (\(Tuple i b') a -> Tuple (i - 1) (f (i - 1) a b'))
            (Tuple len b)
            revList
    where
    Tuple len revList = rev (Tuple 0 nil) xs
      where
      -- As we create our reversed list, we count elements.
      rev = foldl (\(Tuple i acc) a -> Tuple (i + 1) (a : acc))
  foldlWithIndex f acc =
    snd <<< foldl (\(Tuple i b) a -> Tuple (i + 1) (f i b a)) (Tuple 0 acc)
  foldMapWithIndex f = foldlWithIndex (\i acc -> append acc <<< f i) mempty

instance unfoldable1List :: Unfoldable1 List where
  unfoldr1 = go where
    go f b = Z.defer \_ -> case f b of
      Tuple a (Just b') -> a : go f b'
      Tuple a Nothing -> a : nil

instance unfoldableList :: Unfoldable List where
  unfoldr = go where
    go f b = Z.defer \_ -> case f b of
      Nothing -> nil
      Just (Tuple a b') -> a : go f b'

instance traversableList :: Traversable List where
  traverse f =
    foldr (\a b -> cons <$> f a <*> b) (pure nil)

  sequence = traverse identity

instance traversableWithIndexList :: TraversableWithIndex Int List where
  traverseWithIndex f =
    foldrWithIndex (\i a b -> cons <$> f i a <*> b) (pure nil)

instance applyList :: Apply List where
  apply = ap

instance applicativeList :: Applicative List where
  pure a = a : nil

instance bindList :: Bind List where
  bind xs f = List (go <$> unwrap xs)
    where
    go Nil = Nil
    go (Cons x xs') = step (f x <> bind xs' f)

instance monadList :: Monad List

instance altList :: Alt List where
  alt = append

instance plusList :: Plus List where
  empty = nil

instance alternativeList :: Alternative List

instance monadPlusList :: MonadPlus List

instance extendList :: Extend List where
  extend f l =
    case step l of
      Nil -> nil
      Cons _ as ->
        f l : (foldr go { val: nil, acc: nil } as).val
    where
      go a { val, acc } =
        let acc' = a : acc
        in { val: f acc' : val, acc: acc' }

newtype NonEmptyList a = NonEmptyList (Lazy (NonEmpty List a))

toList :: NonEmptyList ~> List
toList (NonEmptyList nel) = Z.defer \_ ->
  case force nel of x :| xs -> x : xs

derive instance newtypeNonEmptyList :: Newtype (NonEmptyList a) _

derive newtype instance eqNonEmptyList :: Eq a => Eq (NonEmptyList a)
derive newtype instance ordNonEmptyList :: Ord a => Ord (NonEmptyList a)

instance eq1NonEmptyList :: Eq1 NonEmptyList where
  eq1 (NonEmptyList lhs) (NonEmptyList rhs) = eq1 lhs rhs

instance ord1NonEmptyList :: Ord1 NonEmptyList where
  compare1 (NonEmptyList lhs) (NonEmptyList rhs) = compare1 lhs rhs

instance showNonEmptyList :: Show a => Show (NonEmptyList a) where
  show (NonEmptyList nel) = "(NonEmptyList " <> show nel <> ")"

instance functorNonEmptyList :: Functor NonEmptyList where
  map f (NonEmptyList nel) = NonEmptyList (map f <$> nel)

instance applyNonEmptyList :: Apply NonEmptyList where
  apply (NonEmptyList nefs) (NonEmptyList neas) =
    case force nefs, force neas of
      f :| fs, a :| as ->
        NonEmptyList (defer \_ -> f a :| (fs <*> a : nil) <> ((f : fs) <*> as))

instance applicativeNonEmptyList :: Applicative NonEmptyList where
  pure a = NonEmptyList (defer \_ -> NE.singleton a)

instance bindNonEmptyList :: Bind NonEmptyList where
  bind (NonEmptyList nel) f =
    case force nel of
      a :| as ->
        case force $ unwrap $ f a of
          b :| bs ->
            NonEmptyList (defer \_ -> b :| bs <> bind as (toList <<< f))

instance monadNonEmptyList :: Monad NonEmptyList

instance altNonEmptyList :: Alt NonEmptyList where
  alt = append

instance extendNonEmptyList :: Extend NonEmptyList where
  extend f w@(NonEmptyList nel) =
    case force nel of
      _ :| as ->
        NonEmptyList $ defer \_ ->
          f w :| (foldr go { val: nil, acc: nil } as).val
    where
    go a { val, acc } =
      { val: f (NonEmptyList (defer \_ -> a :| acc)) : val
      , acc: a : acc
      }

instance comonadNonEmptyList :: Comonad NonEmptyList where
  extract (NonEmptyList nel) = NE.head $ force nel

instance semigroupNonEmptyList :: Semigroup (NonEmptyList a) where
  append (NonEmptyList neas) as' =
    case force neas of
      a :| as -> NonEmptyList (defer \_ -> a :| as <> toList as')

instance foldableNonEmptyList :: Foldable NonEmptyList where
  foldr f b (NonEmptyList nel) = foldr f b (force nel)
  foldl f b (NonEmptyList nel) = foldl f b (force nel)
  foldMap f (NonEmptyList nel) = foldMap f (force nel)

instance traversableNonEmptyList :: Traversable NonEmptyList where
  traverse f (NonEmptyList nel) =
    map (\xxs -> NonEmptyList $ defer \_ -> xxs) $ traverse f (force nel)
  sequence (NonEmptyList nel) =
    map (\xxs -> NonEmptyList $ defer \_ -> xxs) $ sequence (force nel)

instance unfoldable1NonEmptyList :: Unfoldable1 NonEmptyList where
  unfoldr1 f b = NonEmptyList $ defer \_ -> unfoldr1 f b

instance functorWithIndexNonEmptyList :: FunctorWithIndex Int NonEmptyList where
  mapWithIndex f (NonEmptyList ne) = NonEmptyList $ defer \_ -> mapWithIndex (f <<< maybe 0 (add 1)) $ force ne

instance foldableWithIndexNonEmptyList :: FoldableWithIndex Int NonEmptyList where
  foldMapWithIndex f (NonEmptyList ne) = foldMapWithIndex (f <<< maybe 0 (add 1)) $ force ne
  foldlWithIndex f b (NonEmptyList ne) = foldlWithIndex (f <<< maybe 0 (add 1)) b $ force ne
  foldrWithIndex f b (NonEmptyList ne) = foldrWithIndex (f <<< maybe 0 (add 1)) b $ force ne

instance traversableWithIndexNonEmptyList :: TraversableWithIndex Int NonEmptyList where
  traverseWithIndex f (NonEmptyList ne) =
    map (\xxs -> NonEmptyList $ defer \_ -> xxs) $ traverseWithIndex (f <<< maybe 0 (add 1)) $ force ne
