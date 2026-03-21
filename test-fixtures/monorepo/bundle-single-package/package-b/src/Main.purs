module PackageB.Main where

import Prelude
import Effect (Effect)
import Data.Map as Map

main :: Effect Unit
main = void $ pure (Map.empty :: Map.Map String Int)
