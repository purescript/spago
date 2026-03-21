module PackageC.Main where

import Prelude
import Effect (Effect)
import PackageB.Main as B

main :: Effect Unit
main = B.main
