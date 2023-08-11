module Data.Traversable.Accum
  ( Accum
  ) where

type Accum s a = { accum :: s, value :: a }
