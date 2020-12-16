module Main where

import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)
import Prelude (Unit, ($), bind)
import Data.Array (drop)
import Data.Show (show)

main :: Effect Unit
main = do
  args <- argv
  -- dropping the first two args, node path and script name, to make test stable
  log $ show $ drop 2 args
