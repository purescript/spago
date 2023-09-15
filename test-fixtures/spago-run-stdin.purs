module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.Process (stdin)
import Node.Stream (dataHStr, setEncoding)
import Node.EventEmitter (on_)

main :: Effect Unit
main = do
  setEncoding stdin UTF8
  stdin # on_ dataHStr log
