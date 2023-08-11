module Data.Compactable
  ( class Compactable
  , compact
  , separate
  , compactDefault
  , separateDefault
  , applyMaybe
  , applyEither
  , bindMaybe
  , bindEither
  ) where

import Control.Alternative (empty, (<|>))
import Control.Applicative (class Apply, apply, pure)
import Control.Apply ((<*>))
import Control.Bind (class Bind, bind, join)
import Control.Monad.ST as ST
import Data.Array ((!!))
import Data.Array.ST as STA
import Data.Array.ST.Iterator as STAI
import Data.Either (Either(Right, Left), hush, note)
import Data.Foldable (foldl, foldr)
import Data.Function (($))
import Data.Functor (class Functor, map, (<$>))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Prelude (class Ord, const, discard, unit, void, (<<<))

-- | `Compactable` represents data structures which can be _compacted_/_filtered_.
-- | This is a generalization of catMaybes as a new function `compact`. `compact`
-- | has relations with `Functor`, `Applicative`, `Monad`, `Plus`, and `Traversable`
-- | in that we can use these classes to provide the ability to operate on a data type
-- | by eliminating intermediate Nothings. This is useful for representing the
-- | filtering out of values, or failure.
-- |
-- | To be compactable alone, no laws must be satisfied other than the type signature.
-- |
-- | If the data type is also a Functor the following should hold:
-- |
-- | - Functor Identity: `compact <<< map Just ≡ id`
-- |
-- | According to Kmett, (Compactable f, Functor f) is a functor from the
-- | kleisli category of Maybe to the category of Hask.
-- | `Kleisli Maybe -> Hask`.
-- |
-- | If the data type is also `Applicative` the following should hold:
-- |
-- | - `compact <<< (pure Just <*> _) ≡ id`
-- | - `applyMaybe (pure Just) ≡ id`
-- | - `compact ≡ applyMaybe (pure id)`
-- |
-- | If the data type is also a `Monad` the following should hold:
-- |
-- | - `flip bindMaybe (pure <<< Just) ≡ id`
-- | - `compact <<< (pure <<< (Just (=<<))) ≡ id`
-- | - `compact ≡ flip bindMaybe pure`
-- |
-- | If the data type is also `Plus` the following should hold:
-- |
-- | - `compact empty ≡ empty`
-- | - `compact (const Nothing <$> xs) ≡ empty`

class Compactable f where
  compact :: forall a.
    f (Maybe a) -> f a

  separate :: forall l r.
    f (Either l r) -> { left :: f l, right :: f r }

compactDefault :: forall f a. Functor f => Compactable f =>
  f (Maybe a) -> f a
compactDefault = _.right <<< separate <<< map (note unit)

separateDefault :: forall f l r. Functor f => Compactable f =>
  f (Either l r) -> { left :: f l, right :: f r}
separateDefault xs = { left: compact $ (hush <<< swapEither) <$> xs
                     , right: compact $ hush <$> xs
                     }
  where
    swapEither e = case e of
      Left x  -> Right x
      Right y -> Left y

instance compactableMaybe :: Compactable Maybe where
  compact = join

  separate Nothing = { left: Nothing, right: Nothing }
  separate (Just e) = case e of
    Left l  -> { left: Just l, right: Nothing }
    Right r -> { left: Nothing, right: Just r }

instance compactableEither :: Monoid m => Compactable (Either m) where
  compact (Left m) = Left m
  compact (Right m) = case m of
    Just v  -> Right v
    Nothing -> Left mempty

  separate (Left x) = { left: Left x, right: Left x }
  separate (Right e) = case e of
    Left l  -> { left: Right l, right: Left mempty }
    Right r -> { left: Left mempty, right: Right r }

instance compactableArray :: Compactable Array where
  compact xs = ST.run do
    result <- STA.new
    iter   <- STAI.iterator (xs !! _)

    STAI.iterate iter $ void <<< case _ of
      Nothing -> pure 0
      Just j  -> STA.push j result

    STA.unsafeFreeze result

  separate xs = ST.run do
    ls <- STA.new
    rs <- STA.new
    iter <- STAI.iterator (xs !! _)

    STAI.iterate iter $ void <<< case _ of
      Left l  -> STA.push l ls
      Right r -> STA.push r rs

    {left: _, right: _} <$> STA.unsafeFreeze ls <*> STA.unsafeFreeze rs

instance compactableList :: Compactable List.List where
  compact = List.catMaybes
  separate = foldl go { left: empty, right: empty } where
    go acc = case _ of
      Left l  -> acc { left = acc.left <|> pure l }
      Right r -> acc { right = acc.right <|> pure r }

instance compactableMap :: Ord k => Compactable (Map.Map k) where
  compact = foldr select Map.empty <<< mapToList
    where
      select (Tuple k x) m = Map.alter (const x) k m

  separate = foldr select { left: Map.empty, right: Map.empty } <<< mapToList
    where
      select (Tuple k v) { left, right } = case v of
        Left l -> { left: Map.insert k l left, right }
        Right r -> { left: left, right: Map.insert k r right }

mapToList :: forall k v. Ord k =>
  Map.Map k v -> List.List (Tuple k v)
mapToList = Map.toUnfoldable

applyMaybe :: forall f a b. Apply f => Compactable f =>
  f (a -> Maybe b) -> f a -> f b
applyMaybe p = compact <<< apply p

applyEither :: forall f a l r. Apply f => Compactable f =>
  f (a -> Either l r) -> f a -> { left :: f l, right :: f r }
applyEither p = separate <<< apply p

bindMaybe :: forall m a b. Bind m => Compactable m =>
  m a -> (a -> m (Maybe b)) -> m b
bindMaybe x = compact <<< bind x

bindEither :: forall m a l r. Bind m => Compactable m =>
  m a -> (a -> m (Either l r)) -> { left :: m l, right :: m r }
bindEither x = separate <<< bind x
