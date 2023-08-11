// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Data_Codec from "../Data.Codec/index.js";
import * as Data_Codec_Argonaut from "../Data.Codec.Argonaut/index.js";
import * as Data_Codec_Argonaut_Record from "../Data.Codec.Argonaut.Record/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Map from "../Data.Map/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Set from "../Data.Set/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Record from "../Record/index.js";
import * as Registry_Internal_Codec from "../Registry.Internal.Codec/index.js";
import * as Registry_PackageName from "../Registry.PackageName/index.js";
import * as Registry_Version from "../Registry.Version/index.js";
import * as Spago_Config from "../Spago.Config/index.js";
import * as Spago_Core_Config from "../Spago.Core.Config/index.js";
import * as Spago_Core_Prelude from "../Spago.Core.Prelude/index.js";
import * as Spago_Log from "../Spago.Log/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var object = /* #__PURE__ */ Data_Codec_Argonaut_Record.object();
var rowListCodecCons = /* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(Data_Codec_Argonaut_Record.rowListCodecNil)()();
var object1 = /* #__PURE__ */ object(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ rowListCodecCons({
    reflectSymbol: function () {
        return "path";
    }
}))()()({
    reflectSymbol: function () {
        return "package";
    }
}))()()({
    reflectSymbol: function () {
        return "hasTests";
    }
}));
var $$delete = /* #__PURE__ */ Record["delete"]({
    reflectSymbol: function () {
        return "doc";
    }
})()();
var object2 = /* #__PURE__ */ object(/* #__PURE__ */ rowListCodecCons({
    reflectSymbol: function () {
        return "version";
    }
}));
var alt = /* #__PURE__ */ Control_Alt.alt(Data_Either.altEither);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Either.functorEither);
var object3 = /* #__PURE__ */ object(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ rowListCodecCons({
    reflectSymbol: function () {
        return "value";
    }
}))()()({
    reflectSymbol: function () {
        return "type";
    }
}));
var map2 = /* #__PURE__ */ Data_Functor.map(Data_Map_Internal.functorMap);
var fromFoldable = /* #__PURE__ */ Data_Map_Internal.fromFoldable(Registry_PackageName.ordPackageName)(Data_Foldable.foldableArray);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Spago_Core_Prelude.bindSpago);
var logDebug = /* #__PURE__ */ Spago_Log.logDebug(Spago_Core_Prelude.monadEffectSpago)(Spago_Core_Prelude.monadAskSpago)(Spago_Log.loggableString);
var bind = /* #__PURE__ */ Control_Bind.bind(Spago_Core_Prelude.bindSpago);
var ask = /* #__PURE__ */ Control_Monad_Reader_Class.ask(Spago_Core_Prelude.monadAskSpago);
var die = /* #__PURE__ */ Spago_Log.die(Spago_Core_Prelude.monadEffectSpago)(Spago_Core_Prelude.monadAskSpago)(Spago_Log.loggableString);
var toUnfoldable = /* #__PURE__ */ Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray);
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var filterKeys = /* #__PURE__ */ Data_Map_Internal.filterKeys(Registry_PackageName.ordPackageName);
var elem = /* #__PURE__ */ Data_Foldable.elem(Data_Set.foldableSet)(Registry_PackageName.eqPackageName);
var logWarn = /* #__PURE__ */ Spago_Log.logWarn(Spago_Core_Prelude.monadEffectSpago)(Spago_Core_Prelude.monadAskSpago)(Spago_Log.loggableString);
var formatPackagesTable = function (dictMonadEffect) {
    var output = Spago_Log.output(dictMonadEffect);
    return function (pkgs) {
        var showVersion = function (v) {
            if (v instanceof Spago_Config.RegistryVersion) {
                return Registry_Version.print(v.value0);
            };
            if (v instanceof Spago_Config.GitPackage) {
                return v.value0.ref;
            };
            if (v instanceof Spago_Config.LocalPackage) {
                return "local";
            };
            if (v instanceof Spago_Config.WorkspacePackage) {
                return "workspace";
            };
            throw new Error("Failed pattern match at Spago.Command.Ls (line 138, column 17 - line 142, column 38): " + [ v.constructor.name ]);
        };
        var showLocation = function (v) {
            if (v instanceof Spago_Config.RegistryVersion) {
                return "-";
            };
            if (v instanceof Spago_Config.GitPackage) {
                return v.value0.git;
            };
            if (v instanceof Spago_Config.LocalPackage) {
                return v.value0.path;
            };
            if (v instanceof Spago_Config.WorkspacePackage) {
                return v.value0.path;
            };
            throw new Error("Failed pattern match at Spago.Command.Ls (line 131, column 18 - line 135, column 38): " + [ v.constructor.name ]);
        };
        var toRow = function (v) {
            return [ Registry_PackageName.print(v.value0), showVersion(v.value1), showLocation(v.value1) ];
        };
        return output(new Spago_Log.OutputTable({
            titles: [ "Package", "Version", "Location" ],
            rows: map(toRow)(pkgs)
        }));
    };
};
var formatPackagesTable1 = /* #__PURE__ */ formatPackagesTable(Spago_Core_Prelude.monadEffectSpago);
var formatPackagesJson = function (dictMonadEffect) {
    var output = Spago_Log.output(dictMonadEffect);
    return function (packages) {
        var wrapPackage = function (value) {
            return {
                value: value,
                type: (function () {
                    if (value instanceof Spago_Config.RegistryVersion) {
                        return "registry";
                    };
                    if (value instanceof Spago_Config.GitPackage) {
                        return "git";
                    };
                    if (value instanceof Spago_Config.LocalPackage) {
                        return "local";
                    };
                    if (value instanceof Spago_Config.WorkspacePackage) {
                        return "workspace";
                    };
                    throw new Error("Failed pattern match at Spago.Command.Ls (line 79, column 13 - line 83, column 42): " + [ value.constructor.name ]);
                })()
            };
        };
        var workspacePackageCodec = (function () {
            var encode = (function () {
                var $119 = Data_Codec.encode(object1("WorkspacePackage")({
                    path: Data_Codec_Argonaut.string,
                    "package": Spago_Core_Config.packageConfigCodec,
                    hasTests: Data_Codec_Argonaut["boolean"]
                }));
                var $120 = $$delete(Type_Proxy["Proxy"].value);
                return function ($121) {
                    return $119($120($121));
                };
            })();
            var decode = function (_json) {
                return new Data_Either.Left(Data_Codec_Argonaut.MissingValue.value);
            };
            return Data_Codec["codec$prime"](decode)(encode);
        })();
        var packageCodec = (function () {
            var registryVersionCodec = object2("RegistryVersion")({
                version: Registry_Version.codec
            });
            var encode = function (v) {
                if (v instanceof Spago_Config.RegistryVersion) {
                    return Data_Codec.encode(registryVersionCodec)({
                        version: v.value0
                    });
                };
                if (v instanceof Spago_Config.GitPackage) {
                    return Data_Codec.encode(Spago_Core_Config.gitPackageCodec)(v.value0);
                };
                if (v instanceof Spago_Config.LocalPackage) {
                    return Data_Codec.encode(Spago_Core_Config.localPackageCodec)(v.value0);
                };
                if (v instanceof Spago_Config.WorkspacePackage) {
                    return Data_Codec.encode(workspacePackageCodec)(v.value0);
                };
                throw new Error("Failed pattern match at Spago.Command.Ls (line 93, column 14 - line 97, column 62): " + [ v.constructor.name ]);
            };
            var decode = function (json) {
                return alt(map1(function ($122) {
                    return Spago_Config.RegistryVersion.create((function (v) {
                        return v.version;
                    })($122));
                })(Data_Codec.decode(registryVersionCodec)(json)))(alt(map1(Spago_Config.GitPackage.create)(Data_Codec.decode(Spago_Core_Config.gitPackageCodec)(json)))(alt(map1(Spago_Config.LocalPackage.create)(Data_Codec.decode(Spago_Core_Config.localPackageCodec)(json)))(map1(Spago_Config.WorkspacePackage.create)(Data_Codec.decode(workspacePackageCodec)(json)))));
            };
            var innerCodec = Data_Codec["codec$prime"](decode)(encode);
            return object3("Package")({
                type: Data_Codec_Argonaut.string,
                value: innerCodec
            });
        })();
        return output(new Spago_Log.OutputJson(Registry_Internal_Codec.packageMap(packageCodec), map2(wrapPackage)(fromFoldable(packages))));
    };
};
var formatPackagesJson1 = /* #__PURE__ */ formatPackagesJson(Spago_Core_Prelude.monadEffectSpago);
var listPackageSet = function (v) {
    return discard(logDebug("Running `listPackageSet`"))(function () {
        return bind(ask)(function (v1) {
            if (v1.workspace.packageSet instanceof Spago_Config.Registry) {
                return die("No package set in the project");
            };
            if (v1.workspace.packageSet instanceof Spago_Config.PackageSet) {
                var packages = toUnfoldable(v1.workspace.packageSet.value0);
                if (v.json) {
                    return formatPackagesJson1(packages);
                };
                if (!v.json) {
                    return formatPackagesTable1(packages);
                };
                throw new Error("Failed pattern match at Spago.Command.Ls (line 55, column 7 - line 57, column 46): " + [ v.json.constructor.name ]);
            };
            throw new Error("Failed pattern match at Spago.Command.Ls (line 51, column 3 - line 57, column 46): " + [ v1.workspace.packageSet.constructor.name ]);
        });
    });
};
var listPackages = function (v) {
    return discard(logDebug("Running `listPackages`"))(function () {
        return bind(ask)(function (v1) {
            var direct = Data_Map.keys(unwrap((function (v2) {
                return v2.dependencies;
            })((function (v2) {
                return v2["package"];
            })(v1.selected))));
            var directDependencies = filterKeys(function (v2) {
                return elem(v2)(direct);
            })(v1.dependencies);
            var packages = toUnfoldable((function () {
                if (v.transitive) {
                    return v1.dependencies;
                };
                return directDependencies;
            })());
            if (packages.length === 0) {
                return logWarn("There are no dependencies listed in your configuration");
            };
            if (v.json) {
                return formatPackagesJson1(packages);
            };
            if (!v.json) {
                return formatPackagesTable1(packages);
            };
            throw new Error("Failed pattern match at Spago.Command.Ls (line 70, column 10 - line 72, column 44): " + [ v.json.constructor.name ]);
        });
    });
};
export {
    listPackages,
    listPackageSet
};
