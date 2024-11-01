module Test.Spago.Lock where

import Test.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Map as Map
import Data.Set as Set
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Spago.Core.Config (Dependencies(..), ExtraPackage(..), RemotePackage(..), SetAddress(..))
import Spago.Core.Config as Config
import Spago.Core.Config as Core
import Spago.FS as FS
import Spago.Lock (LockEntry(..), Lockfile)
import Spago.Lock as Lock
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.it "parses lockfile" \_ -> do
    case parseJson Lock.lockfileCodec validLockfileString of
      Left error ->
        Assert.fail $ "Failed to parse: " <> CJ.DecodeError.print error
      Right lock | lock /= validLockfile ->
        Assert.fail ("\n" <> printJson Lock.lockfileCodec lock <> "\ndoes not equal\n\n" <> printJson Lock.lockfileCodec validLockfile)
      Right _ ->
        pure unit

  Spec.it "#1158: always uses forward slash separator for doubly nested projects' paths" \{ spago, fixture, testCwd } -> do
    FS.copyTree { src: fixture "lock/1158-doubly-nested-projects", dst: testCwd </> "." }
    spago [ "install" ] >>= shouldBeSuccess
    checkFixture (testCwd </> "spago.lock") (fixture "lock/1158-doubly-nested-projects/expected-lockfile.txt")

validLockfile :: Lockfile
validLockfile =
  { workspace:
      { packages: Map.fromFoldable
          [ packageTuple "my-app"
              { core:
                  { dependencies: Dependencies $ Map.fromFoldable
                      [ packageTuple "effect" (Just (unsafeFromRight (Range.parse ">=1.0.0 <5.0.0")))
                      , packageTuple "my-library" Nothing
                      ]
                  , build_plan: Set.fromFoldable
                      [ mkPackageName "my-library"
                      , mkPackageName "effect"
                      , mkPackageName "prelude"
                      ]
                  }
              , test:
                  { dependencies: Dependencies Map.empty
                  , build_plan: Set.empty
                  }
              , path: "my-app"
              }
          , packageTuple "my-library"
              { core:
                  { dependencies: Dependencies $ Map.fromFoldable [ packageTuple "prelude" Nothing ]
                  , build_plan: Set.fromFoldable [ mkPackageName "prelude" ]
                  }
              , test:
                  { dependencies: Dependencies $ Map.fromFoldable [ packageTuple "console" (Just Config.widestRange) ]
                  , build_plan: Set.fromFoldable [ mkPackageName "console" ]
                  }
              , path: "my-library"
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
        [ packageTuple "console" $
            FromGit
              { url: "https://github.com/purescript/purescript-console.git"
              , rev: "3b83d7b792d03872afeea5e62b4f686ab0f09842"
              , subdir: Nothing
              , dependencies: [ prelude ]
              }
        , packageTuple "effect" $
            FromRegistry
              { version: unsafeFromRight (Version.parse "4.0.0")
              , integrity: unsafeFromRight (Sha256.parse "sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M=")
              , dependencies: [ prelude ]
              }
        , packageTuple "prelude" $
            FromGit
              { url: "https://github.com/purescript/purescript-libraries.git"
              , rev: "3b83d7b792d03872afeea5e62b4f686ab0f09842"
              , subdir: Just "prelude"
              , dependencies: []
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
{
  "workspace": {
    "packages": {
      "my-app": {
        "path": "my-app",
        "core": {
          "build_plan": [
            "effect",
            "my-library",
            "prelude"
          ],
          "dependencies": [
            {
              "effect": ">=1.0.0 <5.0.0"
            },
            "my-library"
          ]
        },
        "test": {
          "dependencies": [],
          "build_plan": []
        }
      },
      "my-library": {
        "path": "my-library",
        "core": {
          "build_plan": [
            "prelude"
          ],
          "dependencies": [
            "prelude"
          ]
        },
        "test": {
          "dependencies": [
            {
              "console": "*"
            }
          ],
          "build_plan": [
            "console"
          ]
        }
      }
    },
    "package_set": {
      "address": {
        "registry": "22.1.1"
      },
      "compiler": ">=0.13.8 <0.14.0",
      "content": {
        "console": "4.0.0",
        "effect": "4.0.0",
        "prelude": "4.0.0"
      }
    },
    "extra_packages": {
      "console": {
        "git": "https://github.com/purescript/purescript-console.git",
        "ref": "v1.0.0"
      },
      "prelude": {
        "git": "https://github.com/purescript/purescript-libraries.git",
        "ref": "v1.0.0",
        "subdir": "prelude"
      }
    }
  },
  "packages": {
    "console": {
      "type": "git",
      "url": "https://github.com/purescript/purescript-console.git",
      "rev": "3b83d7b792d03872afeea5e62b4f686ab0f09842",
      "dependencies": [
        "prelude"
      ]
    },
    "effect": {
      "type": "registry",
      "version": "4.0.0",
      "integrity": "sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M=",
      "dependencies": [
        "prelude"
      ]
    },
    "prelude": {
      "type": "git",
      "url": "https://github.com/purescript/purescript-libraries.git",
      "rev": "3b83d7b792d03872afeea5e62b4f686ab0f09842",
      "subdir": "prelude",
      "dependencies": []
    }
  }
}
"""
