module Test.Spec.Tree
  ( Tree(..)
  , Item(..)
  , ActionWith
  , bimapTree
  , countTests
  , isAllParallelizable
  , discardUnfocused
  , modifyAroundAction
  , PathItem(..)
  , Path
  , parentSuiteName
  , parentSuite
  ) where

import Prelude

import Control.Monad.State (execState)
import Control.Monad.State as State
import Data.Array (mapMaybe, snoc)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either, either)
import Data.Foldable (class Foldable, all, foldMapDefaultL, foldl, foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, un)
import Data.Traversable (for, for_)

data Tree c a
  = Node (Either String c) (Array (Tree c a))
  | Leaf String (Maybe a)

instance showGroup :: (Show c, Show a) => Show (Tree c a) where
  show (Node nc xs) = "(Node " <> show nc <> " " <> show xs <> ")"
  show (Leaf name t) = "(Leaf " <> show name <> " " <> show t <> ")"

instance eqGroup :: (Eq c, Eq a) => Eq (Tree c a) where
  eq (Node nc1 xs1) (Node nc2 xs2) = nc1 == nc2 && xs1 == xs2
  eq (Leaf n1 t1) (Leaf n2 t2) = n1 == n2 && t1 == t2
  eq _ _ = false

bimapTree :: forall a b c d. (Array String -> a -> b) -> (NonEmptyArray String -> c -> d) -> Tree a c -> Tree b d
bimapTree g f = go []
  where
  go :: Array String -> Tree a c -> Tree b d
  go namePath spec = case spec of
    Node d xs ->
      let
        namePath' = either (snoc namePath) (const namePath) d
      in
        Node (map (g namePath') d) (map (go namePath') xs)
    Leaf n item -> Leaf n (map (f $ NEA.snoc' namePath n) item)

instance treeBifunctor :: Bifunctor Tree where
  bimap g f = bimapTree (const g) (const f)

instance treeFoldable :: Foldable (Tree c) where
  foldr f i (Leaf _ a) = maybe i (\a' -> f a' i) a
  foldr f i (Node _ as) = foldr (\a i' -> foldr f i' a) i as
  foldl f i (Leaf _ a) = maybe i (\a' -> f i a') a
  foldl f i (Node _ as) = foldl (\i' a -> foldl f i' a) i as
  foldMap f = foldMapDefaultL f

type ActionWith m a = a -> m Unit

newtype Item m a = Item
  { isFocused :: Boolean
  , isParallelizable :: Maybe Boolean
  , example :: (ActionWith m a -> m Unit) -> m Unit
  }

derive instance itemNewtype :: Newtype (Item m a) _

instance itemShow :: Show (Item m a) where
  show (Item { isFocused, isParallelizable }) =
    "Item (" <> show { isFocused, isParallelizable, example: "Function" } <> ")"

instance itemEq :: Eq (Item m a) where
  eq (Item a) (Item b) =
    a.isFocused == b.isFocused && a.isParallelizable == b.isParallelizable

-- | Count the total number of tests in a spec
countTests :: forall c t. Array (Tree c t) -> Int
countTests g = execState (for g go) 0
  where
  go (Node _ xs) = for_ xs go
  go (Leaf _ _) = State.modify_ (_ + 1)

-- | Return true if all items in the tree are parallelizable
isAllParallelizable :: forall c m a. Tree c (Item m a) -> Boolean
isAllParallelizable = case _ of
  Node _ xs -> all isAllParallelizable xs
  Leaf _ x -> x == Nothing || (x >>= un Item >>> _.isParallelizable) == Just true

-- | If there is at least one focused element, all paths which don't
-- | lead to a focused element will be remove. otherwise input will
-- | be returned as unchanged.
discardUnfocused :: forall c m a. Array (Tree c (Item m a)) -> Array (Tree c (Item m a))
discardUnfocused ts = case mapMaybe findFocus ts of
  [] -> ts
  r -> r
  where
  findFocus :: Tree c (Item m a) -> Maybe (Tree c (Item m a))
  findFocus (Node n ts') = case mapMaybe findFocus ts' of
    [] -> Nothing
    r -> Just $ Node n r
  findFocus t@(Leaf _ (Just (Item { isFocused }))) = if isFocused then Just t else Nothing
  findFocus (Leaf _ Nothing) = Nothing

-- | Modify around action of an Item
modifyAroundAction :: forall g a b. (ActionWith g a -> ActionWith g b) -> Item g a -> Item g b
modifyAroundAction action (Item item) = Item $ item
  { example = \aroundAction -> item.example (aroundAction <<< action)
  }

newtype PathItem = PathItem { index :: Int, name :: Maybe String }

derive instance newtypePathItem :: Newtype PathItem _
derive newtype instance showIdTerm :: Show PathItem
derive newtype instance pathItemEq :: Eq PathItem
derive newtype instance pathItemOrd :: Ord PathItem

type Path = Array PathItem

parentSuiteName :: Path -> Array String
parentSuiteName = mapMaybe (un PathItem >>> _.name)

parentSuite :: Path -> Maybe { path :: Path, name :: String }
parentSuite = flip foldr Nothing case _, _ of
  PathItem { name: Just name }, Nothing -> Just { path: [], name }
  PathItem { name: Nothing }, Nothing -> Nothing
  p, Just acc -> Just acc { path = [ p ] <> acc.path }
