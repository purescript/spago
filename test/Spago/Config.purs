module Test.Spago.Config where

import Test.Prelude

import Codec.JSON.DecodeError as CJ
import Registry.License as License
import Registry.Location (Location(..))
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Core.Config (SetAddress(..))
import Spago.Core.Config as C
import Spago.Yaml as Yaml
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec Unit
spec =
  Spec.describe "config codec" do
    Spec.it "parses valid spago.yaml" do
      let parsedConfig = Yaml.parseYaml C.configCodec validSpagoYaml.serialized
      case parsedConfig of
        Left err ->
          Assert.fail $ "Failed to parse valid config: " <> show err
        Right parsed | parsed == validSpagoYaml.parsed ->
          pure unit
        Right parsed ->
          Assert.fail $
            "\n-------\nExpected:\n-------\n" <> Yaml.stringifyYaml C.configCodec validSpagoYaml.parsed <>
            "\n\n\n-------\nActual:\n-------\n" <> Yaml.stringifyYaml C.configCodec parsed

    Spec.it "reports errors" do
      Yaml.parseYaml C.configCodec invalidLicenseYaml `shouldFailWith`
        ( "$.package.publish.license: Could not decode PackageConfig:"
        <> "\n  Could not decode PublishConfig:"
        <> "\n    Could not decode License:"
        <> "\n      Invalid SPDX identifier bogus"
        )

    Spec.describe "reports unrecognized fields" do
      Spec.it "under 'package'" do
        Yaml.parseYaml C.configCodec unrecognizedPackageFieldYaml `shouldFailWith`
          ( "$.package: Could not decode PackageConfig:"
          <> "\n  Unknown field(s): bogus_field"
          )

      Spec.it "under 'workspace'" do
        Yaml.parseYaml C.configCodec unrecognizedBuildOptsFieldYaml `shouldFailWith`
          ( "$.workspace.buildOpts: Could not decode WorkspaceConfig:"
          <> "\n  Could not decode WorkspaceBuildOptionsInput:"
          <> "\n    Unknown field(s): bogus_field"
          )
    where
    shouldFailWith result expectedError =
      case result of
        Right _ -> Assert.fail "Expected an error, but parsed successfully"
        Left err -> CJ.print err `shouldEqual` expectedError

validSpagoYaml :: { serialized :: String, parsed :: C.Config }
validSpagoYaml =
  { serialized: """
      package:
        name: testpackage
        publish:
          version: 0.0.0
          license: BSD-3-Clause
          location:
            githubOwner: purescript
            githubRepo: testpackage
        build:
          strict: true
        dependencies:
          - aff
          - prelude
          - console
          - effect
        test:
          main: Test.Main
          dependencies:
            - spec
            - spec-node
      workspace:
        packageSet:
          registry: 56.4.0
      """
  , parsed:
      { package: Just
        { name: unsafeFromRight $ PackageName.parse "testpackage"
        , publish: Just
          { version: unsafeFromRight $ Version.parse "0.0.0"
          , license: unsafeFromRight $ License.parse "BSD-3-Clause"
          , location: Just $ GitHub { owner: "purescript", repo: "testpackage", subdir: Nothing }
          , include: Nothing
          , exclude: Nothing
          }
        , build: Just
          { strict: Just true
          , censorProjectWarnings: Nothing
          , pedanticPackages: Nothing
          }
        , bundle: Nothing
        , run: Nothing
        , description: Nothing
        , dependencies: mkDependencies [ "aff", "prelude", "console", "effect" ]
        , test: Just
          { main: "Test.Main"
          , dependencies: mkDependencies [ "spec", "spec-node" ]
          , censorTestWarnings: Nothing
          , execArgs: Nothing
          , strict: Nothing
          , pedanticPackages: Nothing
          }
        }
      , workspace: Just
        { packageSet: Just $ SetFromRegistry { registry: unsafeFromRight $ Version.parse "56.4.0" }
        , extraPackages: Nothing
        , backend: Nothing
        , buildOpts: Nothing
        }
      }
  }

invalidLicenseYaml :: String
invalidLicenseYaml = """
  package:
    name: spago
    dependencies: []
    publish:
      version: 0.0.0
      license: bogus
"""

unrecognizedPackageFieldYaml :: String
unrecognizedPackageFieldYaml = """
  package:
    name: spago
    dependencies: []
    bogus_field: bogus_value
"""

unrecognizedBuildOptsFieldYaml :: String
unrecognizedBuildOptsFieldYaml = """
  package:
    name: spago
    dependencies: []
  workspace:
    packageSet:
      registry: 56.4.0
    buildOpts:
      bogus_field: bogus_value
"""
