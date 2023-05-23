module Test.Spago.Lock where

import Spago.Prelude

import Data.Codec.Argonaut as CA
import Data.Map as Map
import Registry.PackageName as PackageName
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Spago.Lock (LockEntry(..), Lockfile)
import Spago.Lock as Lock
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec Unit
spec = do
  Spec.it "Parses lockfile" do
    case parseYaml Lock.lockfileCodec validLockfileString of
      Left error ->
        Assert.fail $ "Failed to parse: " <> CA.printJsonDecodeError error
      Right lock | lock /= validLockfile ->
        Assert.fail ("\n" <> printYaml Lock.lockfileCodec lock <> "\ndoes not equal\n\n" <> printYaml Lock.lockfileCodec validLockfile)
      Right _ ->
        pure unit

validLockfile :: Lockfile
validLockfile =
  { packages:
      Map.fromFoldable
        [ packageTuple "console" $ FromGit
            { url: "https://github.com/purescript/purescript-console.git"
            , rev: "3b83d7b792d03872afeea5e62b4f686ab0f09842"
            , subdir: Nothing
            }
        , packageTuple "effect" $ FromRegistry
            { version: unsafeFromRight (Version.parse "4.0.0")
            , integrity: unsafeFromRight (Sha256.parse "sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M=")
            }
        , packageTuple "my-library" $ FromPath
            { path: "my-library"
            }
        , packageTuple "prelude" $ FromGit
            { url: "https://github.com/purescript/purescript-libraries.git"
            , rev: "3b83d7b792d03872afeea5e62b4f686ab0f09842"
            , subdir: Just "prelude"
            }
        ]
  }
  where
  packageTuple name = Tuple (unsafeFromRight (PackageName.parse name))

validLockfileString :: String
validLockfileString =
  """
  packages:
    console:
      type: git
      url: https://github.com/purescript/purescript-console.git
      rev: 3b83d7b792d03872afeea5e62b4f686ab0f09842
    effect:
      type: registry
      version: 4.0.0
      integrity: sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M=
    my-library:
      type: local
      path: my-library
    prelude:
      type: git
      url: https://github.com/purescript/purescript-libraries.git
      rev: 3b83d7b792d03872afeea5e62b4f686ab0f09842
      subdir: prelude
  """
