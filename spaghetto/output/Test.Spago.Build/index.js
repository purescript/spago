// Generated by purs version 0.15.10
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Effect_Aff_Class from "../Effect.Aff.Class/index.js";
import * as Registry_PackageName from "../Registry.PackageName/index.js";
import * as Registry_Version from "../Registry.Version/index.js";
import * as Spago_Command_Init from "../Spago.Command.Init/index.js";
import * as Spago_Core_Config from "../Spago.Core.Config/index.js";
import * as Spago_FS from "../Spago.FS/index.js";
import * as Spago_Prelude from "../Spago.Prelude/index.js";
import * as Test_Prelude from "../Test.Prelude/index.js";
import * as Test_Spec from "../Test.Spec/index.js";
import * as Test_Spec_Assertions from "../Test.Spec.Assertions/index.js";
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit);
var discard1 = /* #__PURE__ */ discard(/* #__PURE__ */ Test_Spec.bindSpecT(Data_Identity.bindIdentity));
var it = /* #__PURE__ */ Test_Spec.it(Data_Identity.monadIdentity)(Test_Spec.exampleFunc);
var discard2 = /* #__PURE__ */ discard(Effect_Aff.bindAff);
var bind = /* #__PURE__ */ Control_Bind.bind(Effect_Aff.bindAff);
var shouldReturn = /* #__PURE__ */ Test_Spec_Assertions.shouldReturn(Effect_Aff.monadThrowAff)(Data_Eq.eqBoolean)(Data_Show.showBoolean);
var exists = /* #__PURE__ */ Spago_FS.exists(Effect_Aff.monadEffectAff);
var mkdirp = /* #__PURE__ */ Spago_FS.mkdirp(Effect_Aff_Class.monadAffAff);
var writeTextFile = /* #__PURE__ */ Spago_FS.writeTextFile(Effect_Aff_Class.monadAffAff);
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var spec = /* #__PURE__ */ Test_Spec.around(Data_Identity.monadIdentity)(Test_Prelude.withTempDir)(/* #__PURE__ */ Test_Spec.describe(Data_Identity.monadIdentity)("build")(/* #__PURE__ */ discard1(/* #__PURE__ */ it("builds successfully")(function (v) {
    return discard2(bind(v.spago([ "init" ]))(Test_Prelude.shouldBeSuccess))(function () {
        return bind(v.spago([ "build" ]))(Test_Prelude.shouldBeSuccess);
    });
}))(function () {
    return discard1(it("passes options to purs")(function (v) {
        return discard2(bind(v.spago([ "init" ]))(Test_Prelude.shouldBeSuccess))(function () {
            return bind(v.spago([ "build", "--purs-args", "--verbose-errors", "--purs-args", "--json-errors" ]))(Test_Prelude.shouldBeSuccess);
        });
    }))(function () {
        return discard1(it("can use a different output folder")(function (v) {
            return discard2(bind(v.spago([ "init" ]))(Test_Prelude.shouldBeSuccess))(function () {
                return discard2(bind(v.spago([ "build", "--output", "myOutput" ]))(Test_Prelude.shouldBeSuccess))(function () {
                    return discard2(shouldReturn(exists("myOutput"))(true))(function () {
                        return shouldReturn(exists("output"))(false);
                    });
                });
            });
        }))(function () {
            return discard1(it("there's only one output folder in a monorepo")(function (v) {
                return discard2(bind(v.spago([ "init" ]))(Test_Prelude.shouldBeSuccess))(function () {
                    return discard2(mkdirp("subpackage/src"))(function () {
                        return discard2(mkdirp("subpackage/test"))(function () {
                            return discard2(writeTextFile("subpackage/src/Main.purs")(Spago_Command_Init.srcMainTemplate("Subpackage.Main")))(function () {
                                return discard2(writeTextFile("subpackage/test/Main.purs")(Spago_Command_Init.testMainTemplate("Subpackage.Test.Main")))(function () {
                                    return discard2(Spago_FS.writeYamlFile(Spago_Core_Config.configCodec)("subpackage/spago.yaml")(Spago_Command_Init.defaultConfig(Spago_Prelude.unsafeFromRight(Registry_PackageName.parse("subpackage")))(Data_Maybe.Nothing.value)("Subpackage.Test.Main")))(function () {
                                        return discard2(bind(v.spago([ "build" ]))(Test_Prelude.shouldBeSuccess))(function () {
                                            return discard2(bind(v.spago([ "build", "-p", "subpackage" ]))(Test_Prelude.shouldBeSuccess))(function () {
                                                return discard2(shouldReturn(exists("output"))(true))(function () {
                                                    return shouldReturn(exists("subpackage/output"))(false);
                                                });
                                            });
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
            }))(function () {
                return discard1(it("fails when there are imports from transitive dependencies and --pedantic-packages is passed")(function (v) {
                    return discard2(bind(v.spago([ "init" ]))(Test_Prelude.shouldBeSuccess))(function () {
                        return discard2(bind(v.spago([ "install" ]))(Test_Prelude.shouldBeSuccess))(function () {
                            return discard2(writeTextFile("src/Main.purs")("module Main where\x0aimport Prelude\x0aimport Data.Maybe\x0aimport Data.List\x0amain = unit"))(function () {
                                return discard2(bind(v.spago([ "build" ]))(Test_Prelude.shouldBeSuccess))(function () {
                                    return bind(v.spago([ "build", "--pedantic-packages" ]))(Test_Prelude.shouldBeFailureErr(v.fixture("check-direct-import-transitive-dependency.txt")));
                                });
                            });
                        });
                    });
                }))(function () {
                    return discard1(it("--pedantic-packages also warns about unused dependencies")(function (v) {
                        return discard2(bind(v.spago([ "init" ]))(Test_Prelude.shouldBeSuccess))(function () {
                            return discard2(writeTextFile("src/Main.purs")("module Main where\x0aimport Prelude\x0amain = unit"))(function () {
                                return discard2(bind(v.spago([ "build" ]))(Test_Prelude.shouldBeSuccess))(function () {
                                    return bind(v.spago([ "build", "--pedantic-packages" ]))(Test_Prelude.shouldBeFailureErr(v.fixture("check-unused-dependency.txt")));
                                });
                            });
                        });
                    }))(function () {
                        return discard1(it("compiles with the specified backend")(function (v) {
                            return discard2(bind(v.spago([ "init" ]))(Test_Prelude.shouldBeSuccess))(function () {
                                var conf = Spago_Command_Init.defaultConfig(Spago_Prelude.unsafeFromRight(Registry_PackageName.parse("subpackage")))(new Data_Maybe.Just(Spago_Prelude.unsafeFromRight(Registry_Version.parse("0.0.1"))))("Test.Main");
                                return discard2(Spago_FS.writeYamlFile(Spago_Core_Config.configCodec)("spago.yaml")({
                                    "package": conf["package"],
                                    workspace: map(function (v2) {
                                        return {
                                            backend: new Data_Maybe.Just({
                                                cmd: "echo",
                                                args: new Data_Maybe.Just([ "hello" ])
                                            }),
                                            build_opts: v2.build_opts,
                                            extra_packages: v2.extra_packages,
                                            lock: v2.lock,
                                            package_set: v2.package_set
                                        };
                                    })(conf.workspace)
                                }))(function () {
                                    return discard2(bind(v.spago([ "build" ]))(Test_Prelude.shouldBeSuccess))(function () {
                                        return bind(v.spago([ "run" ]))(Test_Prelude.shouldBeSuccessErr(v.fixture("alternate-backend-output.txt")));
                                    });
                                });
                            });
                        }))(function () {
                            return it("passing the --codegen flag to purs fails")(function (v) {
                                return discard2(bind(v.spago([ "init" ]))(Test_Prelude.shouldBeSuccess))(function () {
                                    return discard2(bind(v.spago([ "build" ]))(Test_Prelude.shouldBeSuccess))(function () {
                                        return bind(v.spago([ "build", "--purs-args", "--codegen", "--purs-args", "corefn" ]))(Test_Prelude.shouldBeFailureErr(v.fixture("codegen-opt.txt")));
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });
    });
})));
export {
    spec
};
