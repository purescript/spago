module Test.Spago.Lock where

import Test.Prelude

import Data.Array.NonEmpty as NEA
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Spago.Core.Config (Dependencies(..), ExtraPackage(..), RemotePackage(..), SetAddress(..))
import Spago.Core.Config as Config
import Spago.Core.Config as Core
import Spago.Lock (LockEntryData(..), Lockfile)
import Spago.Lock as Lock
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec Unit
spec = do
  Spec.it "parses lockfile" do
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
              , needed_by: mempty
              }
          , packageTuple "my-library"
              { dependencies: Dependencies $ Map.fromFoldable [ packageTuple "prelude" Nothing ]
              , test_dependencies: Dependencies $ Map.fromFoldable [ packageTuple "console" (Just Config.widestRange) ]
              , path: "my-library"
              , needed_by: [ mkPackageName "my-app" ]
              }
          ]
      , package_set: Just
          { address: SetFromRegistry { registry: unsafeFromRight (Version.parse "22.1.1") }
          , compiler: unsafeFromRight (Range.parse ">=0.13.8 <0.14.0")
          -- This is not actually the content of the package set, but you get the idea
          , content: Map.fromFoldable
              [ Tuple (mkPackageName "effect") (Core.RemoteRegistryVersion $ mkVersion "4.0.0")
              , Tuple (mkPackageName "prelude") (Core.RemoteRegistryVersion $ mkVersion "4.0.0")
              , Tuple (mkPackageName "console") (Core.RemoteRegistryVersion $ mkVersion "4.0.0")
              ]
          }
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
        [ packageTuple "console"
            { package:
                FromGit
                  { url: "https://github.com/purescript/purescript-console.git"
                  , rev: "3b83d7b792d03872afeea5e62b4f686ab0f09842"
                  , subdir: Nothing
                  , dependencies: [ prelude ]
                  }
            , needed_by: NEA.singleton (mkPackageName "my-library")
            }
        , packageTuple "effect"
            { package: FromRegistry
                { version: unsafeFromRight (Version.parse "4.0.0")
                , integrity: unsafeFromRight (Sha256.parse "sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M=")
                , dependencies: [ prelude ]
                }
            , needed_by: NEA.singleton (mkPackageName "my-app")
            }
        , packageTuple "prelude"
            { package: FromGit
                { url: "https://github.com/purescript/purescript-libraries.git"
                , rev: "3b83d7b792d03872afeea5e62b4f686ab0f09842"
                , subdir: Just "prelude"
                , dependencies: []
                }
            , needed_by: unsafeFromJust $ NEA.fromFoldable
                [ mkPackageName "my-app"
                , mkPackageName "my-library"
                ]
            }
        ]
  }
  where
  prelude :: PackageName
  prelude = mkPackageName "prelude"

  packageTuple :: forall a. String -> a -> Tuple PackageName a
  packageTuple name = Tuple (mkPackageName name)

validLockfileString :: String
validLockfileString =
  """
workspace:
  packages:
    my-app:
      path: my-app
      needed_by: []
      dependencies:
        - effect: ">=1.0.0 <5.0.0"
        - my-library
      test_dependencies: []

    my-library:
      path: my-library
      needed_by:
        - my-app
      dependencies:
        - prelude
      test_dependencies:
        - console: "*"

  package_set:
    address:
      registry: 22.1.1
    compiler: ">=0.13.8 <0.14.0"
    content:
      console: 4.0.0
      effect: 4.0.0
      prelude: 4.0.0
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
    needed_by:
      - my-library
    package:
      type: git
      url: https://github.com/purescript/purescript-console.git
      rev: 3b83d7b792d03872afeea5e62b4f686ab0f09842
      dependencies:
        - prelude
  effect:
    needed_by:
      - my-app
    package:
      type: registry
      version: 4.0.0
      integrity: sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M=
      dependencies:
        - prelude
  prelude:
    needed_by:
      - my-app
      - my-library
    package:
      type: git
      url: https://github.com/purescript/purescript-libraries.git
      rev: 3b83d7b792d03872afeea5e62b4f686ab0f09842
      subdir: prelude
      dependencies: []
  """
