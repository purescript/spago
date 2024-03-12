module Main where

import Prelude

import Effect (Effect)
import Warn (test)

main :: Effect Unit
main = do
  test 1
