module Test.Spago.Uninstall where

import Test.Prelude

import Data.String as String
import Node.Path as Path
import Spago.Command.Init (DefaultConfigOptions(..))
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec ∷ Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "uninstall" do

    Spec.it "fails when no package was selected" \{ spago, fixture } -> do
      let
        setupSubpackage packageName = do
          let subdir = Path.concat [ packageName, "src" ]
          FS.mkdirp subdir
          FS.writeTextFile (Path.concat [ subdir, "Main.purs" ]) $ "module " <> String.toUpper packageName <> " where"
          FS.writeYamlFile Config.configCodec (Path.concat [ packageName, "spago.yaml" ]) $ mkPackageOnlyConfig
            { packageName: packageName
            , srcDependencies: []
            }
            []
      FS.writeYamlFile Config.configCodec "spago.yaml"
        $ Init.defaultConfig'
        $ WorkspaceOnly { setVersion: Just $ mkVersion "0.0.1" }
      setupSubpackage "foo"
      setupSubpackage "bar"
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "uninstall" ] >>= shouldBeFailureErr (fixture "uninstall-no-package-selection.txt")

    Spec.it "warns when test config does not exist and uninstalling test deps" \{ spago, fixture } -> do
      spago [ "init", "--name", "uninstall-tests" ] >>= shouldBeSuccess
      editSpagoYaml' "spago.yaml" \config ->
        config { package = config.package <#> \p -> p { test = Nothing } }
      spago [ "uninstall", "--test-deps", "either" ] >>= shouldBeSuccessErr (fixture "uninstall-no-test-config.txt")

    Spec.it "warns when packages to uninstall are not declared in source config" \{ spago, fixture } -> do
      spago [ "init", "--name", "uninstall-tests" ] >>= shouldBeSuccess
      spago [ "uninstall", "either" ] >>= shouldBeSuccessErr (fixture "uninstall-deps-undeclared-src-deps.txt")

    Spec.it "warns when packages to uninstall are not declared in test config" \{ spago, fixture } -> do
      spago [ "init", "--name", "uninstall-tests" ] >>= shouldBeSuccess
      spago [ "uninstall", "--test-deps", "either" ] >>= shouldBeSuccessErr (fixture "uninstall-deps-undeclared-test-deps.txt")

    Spec.it "removes declared packages in source config" \{ spago, fixture } -> do
      spago [ "init", "--name", "uninstall-tests" ] >>= shouldBeSuccess
      originalConfig <- FS.readTextFile "spago.yaml"

      spago [ "install", "either" ] >>= shouldBeSuccess
      postInstallConfig <- FS.readTextFile "spago.yaml"
      originalConfig `Assert.shouldNotEqual` postInstallConfig

      spago [ "uninstall", "either" ] >>= shouldBeSuccessErr (fixture "uninstall-remove-src-deps.txt")
      postUninstallConfig <- FS.readTextFile "spago.yaml"
      originalConfig `Assert.shouldEqual` postUninstallConfig

    Spec.it "removes declared packages in test config" \{ spago, fixture } -> do
      spago [ "init", "--name", "uninstall-tests" ] >>= shouldBeSuccess
      originalConfig <- FS.readTextFile "spago.yaml"

      spago [ "install", "--test-deps", "either" ] >>= shouldBeSuccess
      postInstallConfig <- FS.readTextFile "spago.yaml"
      originalConfig `Assert.shouldNotEqual` postInstallConfig

      spago [ "uninstall", "--test-deps", "either" ] >>= shouldBeSuccessErr (fixture "uninstall-remove-test-deps.txt")
      postUninstallConfig <- FS.readTextFile "spago.yaml"
      originalConfig `Assert.shouldEqual` postUninstallConfig
