module Test.Spago.Lock where

import Spago.Prelude

import Data.Codec.Argonaut as CA
import Data.Map as Map
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Spago.Core.Config (Dependencies(..), ExtraPackage(..), RemotePackage(..), SetAddress(..))
import Spago.Core.Config as Config
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
  { workspace:
      { packages: Map.fromFoldable
          [ packageTuple "my-app"
              { dependencies: Dependencies $ Map.fromFoldable
                  [ packageTuple "effect" (Just (unsafeFromRight (Range.parse ">=1.0.0 <5.0.0")))
                  , packageTuple "my-library" Nothing
                  ]
              , test_dependencies: Dependencies Map.empty
              , path: "my-app"
              }
          , packageTuple "my-library"
              { dependencies: Dependencies $ Map.fromFoldable [ packageTuple "prelude" Nothing ]
              , test_dependencies: Dependencies $ Map.fromFoldable [ packageTuple "console" (Just Config.widestRange) ]
              , path: "my-library"
              }
          ]
      , package_set: Just $ SetFromRegistry { registry: unsafeFromRight (Version.parse "22.1.1") }
      , extra_packages: Map.fromFoldable
          [ packageTuple "console" $ ExtraRemotePackage $ RemoteGitPackage
              { git: "https://github.com/purescript/purescript-console.git"
              , ref: "v1.0.0"
              , dependencies: Nothing
              , subdir: Nothing
              }
          , packageTuple "prelude" $ ExtraRemotePackage $ RemoteGitPackage
              { git: "https://github.com/purescript/purescript-libraries.git"
              , ref: "v1.0.0"
              , dependencies: Nothing
              , subdir: Just "prelude"
              }
          ]
      }
  , packages:
      Map.fromFoldable
        [ packageTuple "console" $ FromGit
            { url: "https://github.com/purescript/purescript-console.git"
            , rev: "3b83d7b792d03872afeea5e62b4f686ab0f09842"
            , subdir: Nothing
            , dependencies: [ prelude ]
            }
        , packageTuple "effect" $ FromRegistry
            { version: unsafeFromRight (Version.parse "4.0.0")
            , integrity: unsafeFromRight (Sha256.parse "sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M=")
            , dependencies: [ prelude ]
            }
        , packageTuple "prelude" $ FromGit
            { url: "https://github.com/purescript/purescript-libraries.git"
            , rev: "3b83d7b792d03872afeea5e62b4f686ab0f09842"
            , subdir: Just "prelude"
            , dependencies: []
            }
        ]
  }
  where
  prelude :: PackageName
  prelude = unsafeFromRight (PackageName.parse "prelude")

  packageTuple :: forall a. String -> a -> Tuple PackageName a
  packageTuple name = Tuple (unsafeFromRight (PackageName.parse name))

validLockfileString :: String
validLockfileString =
  """
workspace:
  packages:
    my-app:
      path: my-app
      dependencies:
        - effect: ">=1.0.0 <5.0.0"
        - my-library
      test_dependencies: []

    my-library:
      path: my-library
      dependencies:
        - prelude
      test_dependencies:
        - console: "*"

  package_set:
    registry: 22.1.1

  extra_packages:
    console:
      git: https://github.com/purescript/purescript-console.git
      ref: v1.0.0

    prelude:
      git: https://github.com/purescript/purescript-libraries.git
      ref: v1.0.0
      subdir: prelude

packages:
  console:
    type: git
    url: https://github.com/purescript/purescript-console.git
    rev: 3b83d7b792d03872afeea5e62b4f686ab0f09842
    dependencies:
      - prelude
  effect:
    type: registry
    version: 4.0.0
    integrity: sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M=
    dependencies:
      - prelude
  prelude:
    type: git
    url: https://github.com/purescript/purescript-libraries.git
    rev: 3b83d7b792d03872afeea5e62b4f686ab0f09842
    subdir: prelude
    dependencies: []
  """
