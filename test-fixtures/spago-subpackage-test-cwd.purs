module Subpackage.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Process as Process

main :: Effect Unit
main = do
  Process.cwd >>= log
