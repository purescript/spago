-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa-utils
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa.Util where

import Prelude

import Data.String as Str

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
