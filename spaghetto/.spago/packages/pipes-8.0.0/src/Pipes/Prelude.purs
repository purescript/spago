module Pipes.Prelude where

import Prelude hiding (map)
import Prelude as Prelude
import Pipes
  ( (>->)
  , (>~)
  , await
  , each
  , cat
  , for
  , next
  , yield
  )
import Pipes as Pipes
import Pipes.Core (Consumer_, Pipe, Producer, Producer_)
import Pipes.Internal (Proxy(..), closed)
import Data.List (List(..), (:))
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Tuple (Tuple(..))
import Control.Monad.Trans.Class (lift)

-- | Repeat a monadic action indefinitely, `yield`ing each result
repeatM :: forall a m r. Monad m => m a -> Producer_ a m r
repeatM m = lift m >~ cat

-- | Repeat a monadic action a fixed number of times, `yield`ing each result
replicateM :: forall a m. Monad m => Int -> m a -> Producer_ a m Unit
replicateM n m = lift m >~ take n

-- | Consume all values using a monadic function
mapM_ :: forall a m r. Monad m => (a -> m Unit) -> Consumer_ a m r
mapM_ f = for cat (\a -> lift (f a))

-- | `discard` all incoming values
drain :: forall a m r. Monad m => Consumer_ a m r
drain = for cat Pipes.discard

-- | Apply a function to all values flowing downstream
map :: forall a b m r. Monad m => (a -> b) -> Pipe a b m r
map f = for cat (\a -> yield (f a))

-- | Apply a monadic function to all values flowing downstream
mapM :: forall a b m r. Monad m => (a -> m b) -> Pipe a b m r
mapM f = for cat $ \a -> do
    b <- lift (f a)
    yield b

-- | Convert a stream of actions to a stream of values
sequence :: forall a m r. Monad m => Pipe (m a) a m r
sequence = mapM identity

{- | Apply a function to all values flowing downstream, and
     forward each element of the result.
-}
mapFoldable :: forall a b m t r
             . Monad m
            => Foldable t
            => (a -> t b)
            -> Pipe a b m r
mapFoldable f = for cat (\a -> each (f a))


-- | `filter` only forwards values that satisfy the predicate.
filter :: forall a m r. Monad m => (a -> Boolean) -> Pipe a a m r
filter predicate = for cat $ \a -> when (predicate a) (yield a)

-- |`filterM` only forwards values that satisfy the monadic predicate
filterM :: forall a m r. Monad m => (a -> m Boolean) -> Pipe a a m r
filterM predicate = for cat $ \a -> do
    b <- lift (predicate a)
    when b (yield a)

-- | `take n` only allows n values to pass through
take :: forall a m. Monad m => Int -> Pipe a a m Unit
take = loop where
  loop 0 = pure unit
  loop n = do
    a <- await
    yield a
    loop (n - 1)

{-| `takeWhile` allows values to pass downstream so long as they satisfy
    the predicate `p`.
-}
takeWhile :: forall a m. Monad m => (a -> Boolean) -> Pipe a a m Unit
takeWhile predicate = go
  where
    go = do
        a <- await
        if (predicate a)
            then do
                yield a
                go
            else pure unit

{-| `takeWhile'` is a version of `takeWhile` that returns the value failing
    the predicate.
-}
takeWhile' :: forall a m. Monad m => (a -> Boolean) -> Pipe a a m a
takeWhile' predicate = go
  where
    go = do
        a <- await
        if (predicate a)
            then do
                yield a
                go
            else pure a

-- | drop discards n values going downstream
drop :: forall a m r. Monad m => Int -> Pipe a a m r
drop = loop
  where
    loop 0 = cat
    loop n =  do
      _ <- await
      loop (n-1)

{-| dropWhile discards values going downstream until one violates the
    predicate p.
-}
dropWhile :: forall a m r. Monad m => (a -> Boolean) -> Pipe a a m r
dropWhile predicate = go
  where
    go = do
        a <- await
        if (predicate a)
            then go
            else do
                yield a
                cat

-- | Flatten all 'Foldable' elements flowing downstream
concat :: forall a m f r. Monad m => Foldable f => Pipe (f a) a m r
concat = for cat each

-- | Outputs the indices of all elements that satisfied the predicate
findIndices :: forall a m r. Monad m => (a -> Boolean) -> Pipe a Int m r
findIndices predicate = go 0
  where
    go n = do
        a <- await
        when (predicate a) (yield n)
        go $ n + 1

-- | Left scan
scan
  :: forall a b x m r
   . Monad m
  => (x -> a -> x) -> x -> (x -> b) -> Pipe a b m r
scan step begin done = go begin
  where
    go x = do
        yield (done x)
        a <- await
        let x' = step x a
        go $ x'

-- | Monadic left scan
scanM
  :: forall a b x m r
   . Monad m
  => (x -> a -> m x) -> m x -> (x -> m b) -> Pipe a b m r
scanM step begin done = do
    x <- lift begin
    go x
  where
    go x = do
        b <- lift (done x)
        yield b
        a  <- await
        x' <- lift (step x a)
        go $ x'

-- | Apply an action to all values flowing downstream
chain :: forall a m r. Monad m => (a -> m Unit) -> Pipe a a m r
chain f = for cat $ \a -> do
    lift (f a)
    yield a

-- | Convert `Show`able values to `String`s
show :: forall a m r. Monad m => Show a => Pipe a String m r
show = map Prelude.show

-- | Evaluate all values flowing downstream to WHNF
-- | XXX: Is this needed in purescript?
seq :: forall a m r. Monad m => Pipe a a m r
seq = for cat $ \a -> yield $ a

-- | Fold of the elements of a `Producer`
fold
  :: forall a b x m
   . Monad m
  => (x -> a -> x) -> x -> (x -> b) -> Producer a m Unit -> m b
fold step begin done p0 = go p0 begin
  where
    go p x = case p of
        Request v  _  -> closed v
        Respond a  fu -> go (fu unit) $ step x a
        M          m  -> m >>= \p' -> go p' x
        Pure    _     -> pure (done x)

-- | Fold of the elements of a `Producer` that preserves the return value
fold'
  :: forall a b x m r
   . Monad m
  => (x -> a -> x) -> x -> (x -> b) -> Producer a m r -> m (Tuple b r)
fold' step begin done p0 = go p0 begin
  where
    go p x = case p of
        Request v  _  -> closed v
        Respond a  fu -> go (fu unit) $ step x a
        M          m  -> m >>= \p' -> go p' x
        Pure    r     -> pure $ Tuple (done x) r

-- | Monadic fold of the elements of a `Producer`
foldM
  :: forall a b x m
   . Monad m
  => (x -> a -> m x) -> m x -> (x -> m b) -> Producer a m Unit -> m b
foldM step begin done p0 = do
    x0 <- begin
    go p0 x0
  where
    go p x = case p of
        Request v  _  -> closed v
        Respond a  fu -> do
            x' <- step x a
            go (fu unit) $ x'
        M          m  -> m >>= \p' -> go p' x
        Pure    _     -> done x

-- | Monadic fold of the elements of a `Producer`
foldM'
  :: forall a b x m r
   . Monad m
  => (x -> a -> m x) -> m x -> (x -> m b) -> Producer a m r -> m (Tuple b r)
foldM' step begin done p0 = do
    x0 <- begin
    go p0 x0
  where
    go p x = case p of
        Request v  _  -> closed v
        Respond a  fu -> do
            x' <- step x a
            go (fu unit) $ x'
        M          m  -> m >>= \p' -> go p' x
        Pure    r     -> do
            b <- done x
            pure (Tuple b r)

-- | all determines whether all the elements of p satisfy the predicate.
all :: forall a m. Monad m => (a -> Boolean) -> Producer a m Unit -> m Boolean
all predicate p = null $ p >-> filter (\a -> not (predicate a))

-- | any determines whether any element of p satisfies the predicate.
any :: forall a m. Monad m => (a -> Boolean) -> Producer a m Unit -> m Boolean
any predicate p = liftM1 not $ null (p >-> filter predicate)

-- | Determines whether all elements are `True`
and :: forall m. Monad m => Producer Boolean m Unit -> m Boolean
and = all identity

-- | Determines whether any element is `True`
or :: forall m. Monad m => Producer Boolean m Unit -> m Boolean
or = any identity

-- | elem returns `True` if p has an element equal to a, `False` otherwise
elem :: forall a m. Monad m => Eq a => a -> Producer a m Unit -> m Boolean
elem a = any (a == _)

-- | notElem returns `False` if p has an element equal to a, `True` otherwise
notElem :: forall a m. Monad m => Eq a => a -> Producer a m Unit -> m Boolean
notElem a = all (a /= _)

-- | Find the first element of a `Producer` that satisfies the predicate
find :: forall a m. Monad m => (a -> Boolean) -> Producer a m Unit -> m (Maybe a)
find predicate p = head (p >-> filter predicate)

{-| Find the index of the first element of a `Producer` that satisfies the
    predicate
-}
findIndex :: forall a m. Monad m => (a -> Boolean) -> Producer a m Unit -> m (Maybe Int)
findIndex predicate p = head (p >-> findIndices predicate)

-- | Retrieve the first element from a `Producer`
head :: forall a m. Monad m => Producer a m Unit -> m (Maybe a)
head p = do
    x <- next p
    pure $ case x of
        Left   _          -> Nothing
        Right (Tuple a _) -> Just a

-- | Index into a `Producer`
index :: forall a m. Monad m => Int -> Producer a m Unit -> m (Maybe a)
index n p = head (p >-> drop n)

-- | Retrieve the last element from a `Producer`
last :: forall a m. Monad m => Producer a m Unit -> m (Maybe a)
last p0 = do
    x <- next p0
    case x of
        Left   _           -> pure Nothing
        Right (Tuple a p') -> go a p'
  where
    go a p = do
        x <- next p
        case x of
            Left   _            -> pure (Just a)
            Right (Tuple a' p') -> go a' p'

-- | Count the number of elements in a `Producer`
length :: forall a m. Monad m => Producer a m Unit -> m Int
length = fold (\n _ -> n + 1) 0 identity

-- | Find the maximum element of a `Producer`
maximum :: forall a m. Monad m => Ord a => Producer a m Unit -> m (Maybe a)
maximum = fold step Nothing identity
  where
    step x a = Just $ case x of
        Nothing -> a
        Just a' -> max a a'
    max x y | x >= y    = x
            | otherwise = y

-- | Find the minimum element of a `Producer`
minimum :: forall a m. Monad m => Ord a => Producer a m Unit -> m (Maybe a)
minimum = fold step Nothing identity
  where
    step x a = Just $ case x of
        Nothing -> a
        Just a' -> min a a'
    min x y | x < y     = x
            | otherwise = y

-- | Determine if a `Producer` is empty
null :: forall a m. Monad m => Producer a m Unit -> m Boolean
null p = do
    x <- next p
    pure $ case x of
        Left  _ -> true
        Right _ -> false

-- | Convert a pure `Producer` into a list
toList :: forall a. Producer a Identity Unit -> List a
toList prod0 = (go prod0) (:) Nil
  where
    go prod _ nil =
      case prod of
        Request v _  -> closed v
        Respond a fu -> Cons a (go (fu unit) Cons nil)
        M         m  -> go (unwrap m) Cons nil
        Pure    _    -> nil

toListM :: forall a m. Monad m => Producer a m Unit -> m (List a)
toListM = fold step begin done
  where
    step x a = x <<< (a : _)
    begin = identity
    done x = x Nil
