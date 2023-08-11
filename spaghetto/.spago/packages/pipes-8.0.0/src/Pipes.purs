module Pipes where

import Prelude (class Monad, Unit, const, flip, pure, unit, (*>), (>>=))
import Pipes.Core (Consumer_, Pipe, Producer, Producer_, closed, pull, request, respond, (+>>), (//>), (/>/), (>\\))
import Pipes.Internal (Proxy(..))
import Data.Foldable (class Foldable)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Foldable as F

infixl 4 composeLoopBodies' as <~
infixr 4 composeLoopBodies  as ~>
infixr 5 replaceAwait       as >~
infixl 5 replaceAwait'      as ~<
infixl 7 composePipes       as >->
infixr 7 composePipes'      as <-<

for
  :: forall a b b' c c' x x' m
   . Monad m
  =>       Proxy x' x b' b m a
  -> (b -> Proxy x' x c' c m b')
  ->       Proxy x' x c' c m a
for = (//>)

-- (~>)
composeLoopBodies
  :: forall a a' b b' c c' x x' m
   . Monad m
  => (a -> Proxy x' x b' b m a')
  -> (b -> Proxy x' x c' c m b')
  -> (a -> Proxy x' x c' c m a')
composeLoopBodies = (/>/)

-- (<~)
composeLoopBodies'
  :: forall a a' b b' c c' x x' m
   . Monad m
  => (b -> Proxy x' x c' c m b')
  -> (a -> Proxy x' x b' b m a')
  -> (a -> Proxy x' x c' c m a')
composeLoopBodies' = flip composeLoopBodies

await :: forall a m. Monad m => Consumer_ a m a
await = request unit

-- (~<)
replaceAwait
  :: forall a a' b y y' c m
   . Monad m
  => Proxy a'   a y' y m b
  -> Proxy Unit b y' y m c
  -> Proxy a'   a y' y m c
replaceAwait p1 p2 = const p1 >\\ p2

-- (>~)
replaceAwait'
  :: forall a a' b y y' c m
   . Monad m
  => Proxy Unit b y' y m c
  -> Proxy a'   a y' y m b
  -> Proxy a'   a y' y m c
replaceAwait' = flip replaceAwait

cat :: forall a m r. Monad m => Pipe a a m r
cat = pull unit

-- (>->)
composePipes
  :: forall a a' b c c' m r
   . Monad m
  => Proxy a'   a Unit b m r
  -> Proxy Unit b c'   c m r
  -> Proxy a'   a c'   c m r
composePipes p1 p2 = const p1 +>> p2

-- (<-<)
composePipes'
  :: forall a a' b c c' m r
   . Monad m
  => Proxy Unit b c'   c m r
  -> Proxy a'   a Unit b m r
  -> Proxy a'   a c'   c m r
composePipes' = flip composePipes

yield :: forall m a. Monad m => a -> Producer_ a m Unit
yield = respond

{-| Consume the first value from a `Producer`
    `next` either fails with a `Left` if the `Producer` terminates or succeeds
    with a `Right` providing the next value and the remainder of the `Producer`.
-}
next :: forall a m r. Monad m => Producer a m r -> m (Either r (Tuple a (Producer a m r)))
next = go
  where
    go p = case p of
        Request v _  -> closed v
        Respond a fu -> pure (Right (Tuple a (fu unit)))
        M         m  -> m >>= go
        Pure    r    -> pure (Left r)

-- | Convert a `F.Foldable` to a `Producer`
each :: forall a f m. Monad m => Foldable f => f a -> Producer_ a m Unit
each xs = F.foldr (\a p -> yield a *> p) (pure unit) xs

-- | Discards a value
discard :: forall a m. Monad m => a -> m Unit
discard _ = pure unit
