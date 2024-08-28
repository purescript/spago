module Main where

import Prelude

import Effect
import Effect.Console (log)

main :: Effect Unit
main = do
  log 42
  pure unit
