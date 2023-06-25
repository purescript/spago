module Test.Spago.Config where

import Spago.Prelude

import Data.Codec.Argonaut as CA
import Data.Map as Map
import Registry.PackageName as PackageName
import Registry.Version as Version
import Registry.License as License
import Spago.Core.Config (Dependencies(..))
import Spago.Core.Config (Config, configCodec) as Config
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Registry.Location (Location(..))

spec :: Spec Unit
spec = do
  Spec.it "Parses config" do
    case parseYaml Config.configCodec validSpagoYAML of
      Left error ->
        Assert.fail $ "Failed to parse: " <> CA.printJsonDecodeError error
      Right config | config /= validConfig ->
        Assert.fail ("\n" <> printYaml Config.configCodec config <> "\ndoes not equal\n\n" <> printYaml Config.configCodec validConfig)
      Right _ ->
        pure unit

validConfig :: Config.Config
validConfig =
  { package: Just
      { name
      , description: Nothing
      , dependencies: Dependencies (Map.fromFoldable (dependency <$> [ "aff", "affjax" ]))
      , bundle: Nothing
      , run: Nothing
      , test: Nothing
      , publish: Just
        { license
        , version
        , excludeFiles: Just [ "src/**/*Test.purs" ]
        , files: Just [ "src-extra/**/*.purs" ]
        , location: Just $ GitHub { owner: "purescript" , repo: "spago" , subdir: Nothing }
        }
      }
      , workspace: Nothing
  }
  where
  license = unsafeFromRight (License.parse "BSD-3-Clause")
  version = unsafeFromRight (Version.parse "0.93.6")
  name :: PackageName
  name = unsafeFromRight (PackageName.parse "mypackage")
  dependency name = Tuple (unsafeFromRight (PackageName.parse  name)) Nothing

validSpagoYAML :: String
validSpagoYAML =
  """
package:
  name: mypackage
  publish:
    version: 0.93.6
    license: BSD-3-Clause
    location:
      githubOwner: purescript
      githubRepo: spago
    files:
      - "src-extra/**/*.purs"
    excludeFiles:
      - "src/**/*Test.purs"
  dependencies:
    - aff
    - affjax
  """
