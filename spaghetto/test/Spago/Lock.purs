module Test.Spago.Lock where

import Spago.Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser as Argonaut
import Data.Codec.Argonaut as CA
import Spago.Lock as Lock
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec Unit
spec = do
  Spec.describe "Parses lockfile formats" do
    Spec.it "workspace:" do
      case Lock.parseLockEntry "workspace:./my-library" of
        Left error -> Assert.fail error
        Right (Lock.WorkspaceLock { path }) | path == "./my-library" -> pure unit
        _ -> Assert.fail "Expected 'WorkspaceLock' with path './my-library'"

    Spec.it "git:" do
      case Lock.parseLockEntry "git:https://github.com/purescript/purescript-console.git#3b83d7b792d03872afeea5e62b4f686ab0f09842" of
        Left error -> Assert.fail error
        Right (Lock.GitLock { rev, url, subdir })
          | rev == "3b83d7b792d03872afeea5e62b4f686ab0f09842"
          , url == "https://github.com/purescript/purescript-console.git"
          , subdir == Nothing -> pure unit
        _ -> Assert.fail "Expected 'GitLock'"

      case Lock.parseLockEntry "git:https://github.com/purescript/purescript-console.git#3b83d7b792d03872afeea5e62b4f686ab0f09842?subdir=console" of
        Left error -> Assert.fail error
        Right (Lock.GitLock { rev, url, subdir })
          | rev == "3b83d7b792d03872afeea5e62b4f686ab0f09842"
          , url == "https://github.com/purescript/purescript-console.git"
          , subdir == Just "console" -> pure unit
        _ -> Assert.fail "Expected 'GitLock'"

    Spec.it "registry:" do
      case Lock.parseLockEntry "registry:4.0.0#sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M=" of
        Left error -> Assert.fail error
        Right _ -> pure unit

    Spec.it "lockfile" do
      case CA.decode Lock.lockfileCodec validLockfile of
        Left error -> Assert.fail (CA.printJsonDecodeError error)
        Right _ -> pure unit

validLockfile :: Json
validLockfile = unsafeFromRight $ Argonaut.jsonParser
  """
  {
    "lockfile": "1.0.0",
    "packages": {
      "console": "git:https://github.com/purescript/purescript-console.git#3b83d7b792d03872afeea5e62b4f686ab0f09842",
      "effect": "registry:4.0.0#sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M=",
      "my-library": "workspace:./my-library",
      "prelude": "git:https://github.com/purescript/purescript-libraries.git#3b83d7b792d03872afeea5e62b4f686ab0f09842?subdir=prelude"
    }
  }
  """
