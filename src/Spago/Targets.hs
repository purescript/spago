module Spago.Targets where

import           Spago.Types

mainTarget :: TargetName
mainTarget = TargetName "main"

testTarget :: TargetName
testTarget = TargetName "test"

examplesTarget :: TargetName
examplesTarget = TargetName "examples"

benchmarkTarget :: TargetName
benchmarkTarget = TargetName "benchmark"
