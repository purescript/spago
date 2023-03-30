-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa-utils
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa.Util where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.String as Str
import Data.Tuple (Tuple(..), snd)

replicate :: forall m. (Monoid m) => Int -> m -> m
replicate n m = go n mempty
  where
  go i x
    | i <= 0 = x
    | otherwise = go (i - 1) (x <> m)

padLeft :: Int -> String -> String
padLeft width str = replicate (width - Str.length str) " " <> str

padRight :: Int -> String -> String
padRight width str = str <> replicate (width - Str.length str) " "

iter_ :: forall m f a b. Foldable f => Applicative m => f a -> (Int -> a -> m b) -> m Unit
iter_ xs f = snd $ foldl go (Tuple 0 (pure unit)) xs
  where
  go (Tuple i a) b = Tuple (i + 1) (a <* f i b)
