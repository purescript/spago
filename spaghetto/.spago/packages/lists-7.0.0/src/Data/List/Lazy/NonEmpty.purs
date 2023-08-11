module Data.List.Lazy.NonEmpty
  ( module Data.List.Lazy.Types
  , toUnfoldable
  , fromFoldable
  , fromList
  , toList
  , singleton
  , repeat
  , iterate
  , head
  , last
  , tail
  , init
  , cons
  , uncons
  , length
  , concatMap
  , appendFoldable
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Lazy (force, defer)
import Data.List.Lazy ((:))
import Data.List.Lazy as L
import Data.List.Lazy.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

toUnfoldable :: forall f. Unfoldable f => NonEmptyList ~> f
toUnfoldable =
  unfoldr (\xs -> (\rec -> Tuple rec.head rec.tail) <$> L.uncons xs) <<< toList

fromFoldable :: forall f a. Foldable f => f a -> Maybe (NonEmptyList a)
fromFoldable = fromList <<< L.fromFoldable

fromList :: forall a. L.List a -> Maybe (NonEmptyList a)
fromList l =
  case L.step l of
    L.Nil -> Nothing
    L.Cons x xs -> Just (NonEmptyList (defer \_ -> x :| xs))

toList :: NonEmptyList ~> L.List
toList (NonEmptyList nel) = case force nel of x :| xs -> x : xs

singleton :: forall a. a -> NonEmptyList a
singleton = pure

repeat :: forall a. a -> NonEmptyList a
repeat x = NonEmptyList $ defer \_ -> x :| L.repeat x

iterate :: forall a. (a -> a) -> a -> NonEmptyList a
iterate f x = NonEmptyList $ defer \_ -> x :| L.iterate f (f x)

head :: forall a. NonEmptyList a -> a
head (NonEmptyList nel) = case force nel of x :| _ -> x

last :: forall a. NonEmptyList a -> a
last (NonEmptyList nel) = case force nel of x :| xs -> fromMaybe x (L.last xs)

tail :: NonEmptyList ~> L.List
tail (NonEmptyList nel) = case force nel of _ :| xs -> xs

init :: NonEmptyList ~> L.List
init (NonEmptyList nel) =
  case force nel of
    x :| xs ->
      maybe L.nil (x : _) (L.init xs)

cons :: forall a. a -> NonEmptyList a -> NonEmptyList a
cons y (NonEmptyList nel) =
  NonEmptyList (defer \_ -> case force nel of x :| xs -> y :| x : xs)

uncons :: forall a. NonEmptyList a -> { head :: a, tail :: L.List a }
uncons (NonEmptyList nel) = case force nel of x :| xs -> { head: x, tail: xs }

length :: forall a. NonEmptyList a -> Int
length (NonEmptyList nel) = case force nel of _ :| xs -> 1 + L.length xs

concatMap :: forall a b. (a -> NonEmptyList b) -> NonEmptyList a -> NonEmptyList b
concatMap = flip bind

appendFoldable :: forall t a. Foldable t => NonEmptyList a -> t a -> NonEmptyList a
appendFoldable nel ys =
  NonEmptyList (defer \_ -> head nel :| tail nel <> L.fromFoldable ys)
