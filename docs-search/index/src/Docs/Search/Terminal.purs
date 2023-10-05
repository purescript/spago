module Docs.Search.Terminal where

import Prelude

bold :: String -> String
bold str =
  "\x1b[1m" <> str <> "\x1b[0m"

yellow :: String -> String
yellow str =
  "\x1b[33m" <> str <> "\x1b[0m"

grey :: String -> String
grey str =
  "\x1b[37m" <> str <> "\x1b[0m"

cyan :: String -> String
cyan str =
  "\x1b[36m" <> str <> "\x1b[0m"

green :: String -> String
green str =
  "\x1b[32m" <> str <> "\x1b[0m"
