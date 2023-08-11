module Dodo.Internal.Buffer where

import Prelude

import Data.List (List(..), drop, foldr, null, (:))

newtype Buffer b = Buffer
  { buffer :: b
  , queue :: List (List (b -> b))
  }

branch :: forall b. Buffer b -> Buffer b
branch (Buffer b) = Buffer b { queue = Nil : b.queue }

commit :: forall b. Buffer b -> Buffer b
commit (Buffer b) = Buffer
  { buffer: foldr (flip (foldr ($))) b.buffer b.queue
  , queue: Nil
  }

revert :: forall b. Buffer b -> Buffer b
revert (Buffer b) = Buffer b { queue = drop 1 b.queue }

modify :: forall b. (b -> b) -> Buffer b -> Buffer b
modify f (Buffer b) = case b.queue of
  fs : queue ->
    Buffer b { queue = (f : fs) : queue }
  _ ->
    Buffer b { buffer = f b.buffer }

get :: forall b. Buffer b -> b
get = commit >>> \(Buffer b) -> b.buffer

new :: forall b. b -> Buffer b
new buffer = Buffer { buffer, queue: Nil }

isBranching :: forall b. Buffer b -> Boolean
isBranching (Buffer b) = not $ null b.queue
