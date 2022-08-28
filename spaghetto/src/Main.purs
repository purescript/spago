module Main where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Console (logShow)
import Toml as Toml

type A = { abc :: { foo :: Int, bar :: Array Int } }

main :: Effect Unit
main = do
  let
    (a :: Either String A) = Toml.parseToml
      """[abc]
      fou = 123
      bar = [1,2,3]
        """
  logShow a