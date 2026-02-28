module Main where

import LocalLibMatch as Match
import LocalLibMismatch as Mismatch
import LocalLibNoVersion as NoVersion

match :: String
match = Match.hello

mismatch :: String
mismatch = Mismatch.hello

noVersion :: String
noVersion = NoVersion.hello
