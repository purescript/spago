module Main where

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.Process (stdin)
import Node.Stream (onDataString)
import Prelude (Unit)

main :: Effect Unit
main = do
  onDataString stdin UTF8 log
