module Test.Spago.Config where

import Test.Prelude

import Codec.JSON.DecodeError as CJ
import Data.String as String
import Registry.License as License
import Registry.Location (Location(..))
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Core.Config (SetAddress(..))
import Spago.Core.Config as C
import Spago.FS as FS
import Spago.Path as Path
import Spago.Paths as Paths
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
          Assert.fail
            $ "\n-------\nExpected:\n-------\n"
            <> Yaml.stringifyYaml C.configCodec validSpagoYaml.parsed
            <> "\n\n\n-------\nActual:\n-------\n"
            <> Yaml.stringifyYaml C.configCodec parsed

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

      Spec.it "under 'publish.location'" do
        Yaml.parseYaml C.configCodec unrecognizedPublishLocationFieldYaml `shouldFailWith`
          ( "$.package.publish.location: Could not decode PackageConfig:"
              <> "\n  Could not decode PublishConfig:"
              <> "\n    Could not decode Publish Location:"
              <> "\n      Failed to decode alternatives:"
              <> "\n        - $.gitUrl: Could not decode Git:"
              <> "\n            No value found"
              <> "\n        - Could not decode GitHub:"
              <> "\n            Unknown field(s): bogus_field"
          )

    Spec.around withTempDir $
      Spec.describe "spago.yaml discovery" do
        Spec.it "discovers config up the directory tree" \{ testCwd, fixture, spago } -> do
          FS.copyTree { src: fixture "config/discovery", dst: testCwd }
          spago [ "build" ] >>= shouldBeSuccess
          spago [ "build" ] >>= shouldBeSuccessErr' (fixture "config/discovery/from-root.txt")

          -- Running from `./a`, Spago should discover the workspace root at
          -- './' and select package 'a'
          Paths.chdir $ testCwd </> "a"
          spago [ "build" ] >>= shouldBeSuccessErr' (fixture "config/discovery/from-a.txt")

          -- Running from `./nested-workspace`, Spago should use the workspace
          -- root at './nested-workspace' and not the one at './'
          Paths.chdir $ testCwd </> "nested-workspace"
          spago [ "build" ] >>= shouldBeSuccess
          spago [ "build" ] >>= shouldBeSuccessErr' (fixture "config/discovery/from-nested.txt")

          -- Running from `./nested-workspace/d`, Spago should use the workspace
          -- root at './nested-workspace', because that's the closest one, and
          -- select package 'd'
          Paths.chdir $ testCwd </> "nested-workspace" </> "d"
          spago [ "build" ] >>= shouldBeSuccessErr' (fixture "config/discovery/from-d.txt")

          -- At workspace roots, a ".spago" directory should be created for
          -- local cache, but not in subdirs
          FS.exists (testCwd </> ".spago") `Assert.shouldReturn` true
          FS.exists (testCwd </> "a" </> ".spago") `Assert.shouldReturn` false
          FS.exists (testCwd </> "nested-workspace" </> ".spago") `Assert.shouldReturn` true
          FS.exists (testCwd </> "nested-workspace" </> "d" </> ".spago") `Assert.shouldReturn` false

        Spec.it "reports no config in any parent directories" \{ spago, fixture } ->
          spago [ "build" ] >>= shouldBeFailureErr' (fixture "config/no-workspace-anywhere.txt")

        Spec.it "reports possible misnamed configs up the directory tree" \{ testCwd, spago, fixture } -> do
          FS.copyTree { src: fixture "config/misnamed-configs", dst: testCwd }
          spago [ "build" ] >>= shouldBeFailureErr' (fixture "config/misnamed-configs/from-root.txt")

          Paths.chdir $ testCwd </> "a"
          spago [ "build" ] >>= shouldBeFailureErr' (fixture "config/misnamed-configs/from-a.txt")

          Paths.chdir $ testCwd </> "a" </> "b"
          spago [ "build" ] >>= shouldBeFailureErr' (fixture "config/misnamed-configs/from-b.txt")

          Paths.chdir $ testCwd </> "a" </> "b" </> "c"
          spago [ "build" ] >>= shouldBeFailureErr' (fixture "config/misnamed-configs/from-c.txt")

          Paths.chdir $ testCwd </> "a" </> "b" </> "d"
          spago [ "build" ] >>= shouldBeFailureErr' (fixture "config/misnamed-configs/from-d.txt")

        Spec.it "warns about a malformed config, but stops parsing down the tree" \{ spago, fixture, testCwd } -> do
          FS.copyTree { src: fixture "config/malformed-configs", dst: testCwd }

          -- Running with "-p bogus" to get Spago to list all available
          -- packages. Packages `b` and `c` shouldn't be in that list because
          -- b's config is malformatted, so Spago should warn about it and stop
          -- loading configs down the tree from `b`, thus skipping `c`.
          spago [ "build", "-p", "bogus" ] >>= checkOutputs'
            { result: isLeft
            , stdoutFile: Nothing
            , stderrFile: Just (fixture "config/malformed-configs/from-root.txt")
            , sanitize: String.trim >>> String.replaceAll (String.Pattern $ Path.toRaw testCwd) (String.Replacement "<test-dir>")
            }

    where
    shouldFailWith result expectedError =
      case result of
        Right _ -> Assert.fail "Expected an error, but parsed successfully"
        Left err -> CJ.print err `shouldEqual` expectedError

    shouldBeSuccessErr' = shouldBeErr isRight
    shouldBeFailureErr' = shouldBeErr isLeft

    shouldBeErr result file = checkOutputs'
        { stdoutFile: Nothing
        , stderrFile: Just file
        , result
        , sanitize:
            String.trim
              >>> String.replaceAll (String.Pattern "\\") (String.Replacement "/")
              >>> String.replaceAll (String.Pattern "\r\n") (String.Replacement "\n")
        }

validSpagoYaml :: { serialized :: String, parsed :: C.Config }
validSpagoYaml =
  { serialized:
      """
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
              , owners: Nothing
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
invalidLicenseYaml =
  """
  package:
    name: spago
    dependencies: []
    publish:
      version: 0.0.0
      license: bogus
"""

unrecognizedPackageFieldYaml :: String
unrecognizedPackageFieldYaml =
  """
  package:
    name: spago
    dependencies: []
    bogus_field: bogus_value
"""

unrecognizedBuildOptsFieldYaml :: String
unrecognizedBuildOptsFieldYaml =
  """
  package:
    name: spago
    dependencies: []
  workspace:
    packageSet:
      registry: 56.4.0
    buildOpts:
      bogus_field: bogus_value
"""

unrecognizedPublishLocationFieldYaml :: String
unrecognizedPublishLocationFieldYaml =
  """
  package:
    name: spago
    dependencies: []
    publish:
      version: 0.0.0
      license: MIT
      location:
        githubOwner: purescript
        githubRepo: spago
        bogus_field: bogus_value
  workspace:
    packageSet:
      registry: 56.4.0
"""
