module Main where

import Prelude
import Effect (Effect)
import Node.FS.Sync (writeTextFile)
import Node.Encoding as Encoding

main :: Effect Unit
main = do
  writeTextFile Encoding.UTF8 "spago-script-result.txt" "HELLO\n"
