module Options.Applicative.Help.Levenshtein where
-- (
--     -- editDistance
--   ) where

-- | Calculate the Damerau-Levenshtein edit distance
--   between two lists (strings).
--
--   This is modified from
--   https://wiki.haskell.org/Edit_distance
--   and is originally from Lloyd Allison's paper
--   "Lazy Dynamic-Programming can be Eager"
--
--   It's been changed though from Levenshtein to
--   Damerau-Levenshtein, which treats transposition
--   of adjacent characters as one change instead of
--   two.
import Prelude

import Data.Array (unsafeIndex)
import Data.Array as Array
import Data.Function.Memoize (memoize2)
import Data.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable (minimum)
import Partial.Unsafe (unsafePartial)


editDistance :: forall a. Eq a => Array a -> Array a -> Int
editDistance xs ys = dist' (Array.length xs) (Array.length ys)
    where
    dist' :: Int -> Int -> Int
    dist' = memoize2 $ \a b -> dist a b

    dist 0 j = j
    dist i 0 = i
    dist i j = minimum $
      (dist' (i-1) j + 1) `NonEmpty`
      [ dist' i (j-1) + 1
      , unsafePartial $ if xs `unsafeIndex` (i - 1) == ys `unsafeIndex` (j - 1)
          then dist' (i-1) (j-1)
          else 1 + dist' (i-1) (j-1)
      ]

-- NOTE(safareli): this uses too much laziness for me, so instead I ported the easier version from https://wiki.haskell.org/Edit_distance
-- -- | Calculate the Damerau-Levenshtein edit distance
-- --   between two lists (strings).

-- --   This is modified from
-- --   https://wiki.haskell.org/Edit_distance
-- --   and is originally from Lloyd Allison's paper
-- --   "Lazy Dynamic-Programming can be Eager"

-- --   It's been changed though from Levenshtein to
-- --   Damerau-Levenshtein, which treats transposition
-- --   of adjacent characters as one change instead of
-- --   two.
-- editDistance :: forall a. Eq a => Array a -> Array a -> Int
-- editDistance a b = last $
--   case unit of
--     _ | lab == 0
--      -> mainDiag
--       | lab > 0
--      -> lowers !! (lab - 1)
--       | otherwise
--      -> uppers !! (-1 - lab)
--   where
--     mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
--     uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
--     lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
--     eachDiag _ [] _ = []
--     eachDiag _ _ [] = []
--     eachDiag a' (_:bs) (lastDiag:diags) =
--       oneDiag a' bs nextDiag lastDiag : eachDiag a' bs diags
--       where
--         nextDiag = head (tail diags)
--     oneDiag a' b' diagAbove diagBelow = thisdiag
--       where
--         doDiag [] _ _ _ _ = []
--         doDiag _ [] _ _ _ = []
--         -- Check for a transposition
--         -- We don't add anything to nw here, the next character
--         -- will be different however and the transposition
--         -- will have an edit distance of 1.
--         doDiag (ach:ach':as) (bch:bch':bs) nw n w
--           | ach' == bch && ach == bch'
--           = nw : (doDiag (ach' : as) (bch' : bs) nw (tail n) (tail w))
--         -- Standard case
--         doDiag (ach:as) (bch:bs) nw n w =
--           me : (doDiag as bs me (tail n) (tail w))
--           where
--             me =
--               if ach == bch
--                 then nw
--                 else 1 + min3 (head w) nw (head n)
--         firstelt = 1 + head diagBelow
--         thisdiag = firstelt : doDiag a' b' firstelt diagAbove (tail diagBelow)
--     lab = length a - length b
--     min3 x y z =
--       if x < y
--         then x
--         else min y z
