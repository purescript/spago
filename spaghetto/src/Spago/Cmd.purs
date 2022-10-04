module Spago.Cmd where

import Spago.Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Spago.Prelude as Maybe

-- | Try to find one of the flags in a list of Purs args
-- TODO: this code needs some comments and a test
findFlag :: { flags :: Array String, args :: Array String } -> Maybe String
findFlag { flags, args } = case Array.uncons args of
  Just { head: x, tail: xs } ->
    if isFlag x then case Array.uncons xs of
      Just { head: y } -> Just y
      _ -> Nothing
    else if hasFlag x then case words x of
      [ word ] -> case splitOnEqual word of
        [ _, value ] -> Just value
        _ -> Nothing
      [ _, value, _ ] -> Just value
      _ -> Nothing
    else findFlag { flags, args: xs }
  _ -> Nothing
  where
  words = String.split (Pattern " ")
  splitOnEqual = String.split (Pattern "=")

  isFlag :: String -> Boolean
  isFlag word = Maybe.isJust $ Array.find (_ == word) flags

  hasFlag :: String -> Boolean
  hasFlag a = Maybe.isJust $ Array.find (_ == firstWord) flags
    where
    firstWord = fromMaybe "" $ case Array.uncons (words a) of
      Just { head: h1, tail } -> case tail of
        [] -> case Array.uncons (splitOnEqual h1) of
          Just { head: h2 } -> Just h2
          _ -> Nothing
        _ -> Just h1
      _ -> Nothing
