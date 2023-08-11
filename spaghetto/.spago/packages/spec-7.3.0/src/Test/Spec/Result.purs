module Test.Spec.Result where

import Prelude

import Data.Function (on)
import Data.Time.Duration (Milliseconds)
import Effect.Exception (Error)
import Effect.Exception as Error
import Test.Spec.Speed (Speed)

data Result
  = Success Speed Milliseconds
  | Failure Error

instance showResult :: Show Result where
  show (Success speed duration ) = "Success (" <> show speed <> " " <> show duration <> ")"
  show (Failure err) = "Failure (Error " <> Error.message err <> ")"

instance eqResult :: Eq Result where
  eq (Success s1 d1) (Success s2 d2) = s1 == s2 && d1 == d2
  eq (Failure err1) (Failure err2) = on (==) Error.message err1 err2 && on (==) Error.stack err1 err2
  eq _ _ = false
