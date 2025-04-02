module PackageA
  ( sample
  )
where

import Data.Maybe (Maybe(..))
import PackageB (exampleFunc)

sample :: forall a. Maybe a
sample =
  case exampleFunc of
    Just val -> Just val
    Nothing -> Nothing

