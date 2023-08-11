module Data.Array.ST.Iterator
  ( Iterator
  , iterator
  , iterate
  , next
  , peek
  , exhausted
  , pushWhile
  , pushAll
  ) where

import Prelude
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array.ST (STArray)
import Data.Array.ST as STA

import Data.Maybe (Maybe(..), isNothing)

-- | This type provides a slightly easier way of iterating over an array's
-- | elements in an STArray computation, without having to keep track of
-- | indices.
data Iterator r a = Iterator (Int -> Maybe a) (STRef r Int)

-- | Make an Iterator given an indexing function into an array (or anything
-- | else). If `xs :: Array a`, the standard way to create an iterator over
-- | `xs` is to use `iterator (xs !! _)`, where `(!!)` comes from `Data.Array`.
iterator :: forall r a. (Int -> Maybe a) -> ST r (Iterator r a)
iterator f =
  Iterator f <$> STRef.new 0

-- | Perform an action once for each item left in an iterator. If the action
-- | itself also advances the same iterator, `iterate` will miss those items
-- | out.
iterate :: forall r a. Iterator r a -> (a -> ST r Unit) -> ST r Unit
iterate iter f = do
  break <- STRef.new false
  ST.while (not <$> STRef.read break) do
    mx <- next iter
    case mx of
      Just x -> f x
      Nothing -> void $ STRef.write true break

-- | Get the next item out of an iterator, advancing it. Returns Nothing if the
-- | Iterator is exhausted.
next :: forall r a. Iterator r a -> ST r (Maybe a)
next (Iterator f currentIndex) = do
  i <- STRef.read currentIndex
  _ <- STRef.modify (_ + 1) currentIndex
  pure (f i)

-- | Get the next item out of an iterator without advancing it.
peek :: forall r a. Iterator r a -> ST r (Maybe a)
peek (Iterator f currentIndex) = do
  i <- STRef.read currentIndex
  pure (f i)

-- | Check whether an iterator has been exhausted.
exhausted :: forall r a. Iterator r a -> ST r Boolean
exhausted = map isNothing <<< peek

-- | Extract elements from an iterator and push them on to an STArray for as
-- | long as those elements satisfy a given predicate.
pushWhile :: forall r a. (a -> Boolean) -> Iterator r a -> STArray r a -> ST r Unit
pushWhile p iter array = do
  break <- STRef.new false
  ST.while (not <$> STRef.read break) do
    mx <- peek iter
    case mx of
      Just x | p x -> do
        _ <- STA.push x array
        void $ next iter
      _ ->
        void $ STRef.write true break

-- | Push the entire remaining contents of an iterator onto an STArray.
pushAll :: forall r a. Iterator r a -> STArray r a -> ST r Unit
pushAll = pushWhile (const true)
