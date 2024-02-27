module Main where

import Prelude((<<<))
import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)
import Prelude (Unit, ($), bind)
import Data.Array (drop)
import Data.Show (show)
import Data.Traversable(traverse_)

main :: Effect Unit
main = do
  args <- argv
  -- dropping the first two args, node path and script name, to make test stable
  traverse_ log (drop 2 args)
