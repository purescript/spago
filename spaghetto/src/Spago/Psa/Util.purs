-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa-utils
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa.Util where

import Prelude

import Data.String as Str
import Data.Monoid (power)

padLeft :: Int -> String -> String
padLeft width str = power " " (width - Str.length str) <> str

padRight :: Int -> String -> String
padRight width str = str <> power " " (width - Str.length str)
