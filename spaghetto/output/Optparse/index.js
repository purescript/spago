import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Semigroupoid from "../Control.Semigroupoid/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Options_Applicative_Types from "../Options.Applicative.Types/index.js";
import * as Record from "../Record/index.js";
import * as Record_Builder from "../Record.Builder/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var identity = /* #__PURE__ */ Control_Category.identity(Record_Builder.categoryBuilder);
var map = /* #__PURE__ */ Data_Functor.map(Options_Applicative_Types.parserFunctor);
var apply = /* #__PURE__ */ Control_Apply.apply(Options_Applicative_Types.parserApply);
var composeFlipped = /* #__PURE__ */ Control_Semigroupoid.composeFlipped(Record_Builder.semigroupoidBuilder);
var insert = /* #__PURE__ */ Record_Builder.insert()();
var buildRecordArgsNil = {
    buildRecordArgs: function (v) {
        return function (v1) {
            return new Options_Applicative_Types.NilP(identity);
        };
    }
};
var buildRecordArgs = function (dict) {
    return dict.buildRecordArgs;
};

// this code is adapted from `argparse-basic`
// it builds a record parser from a record of parsers
var fromRecord = function () {
    return function (dictBuildRecordArgs) {
        var $19 = map(Record_Builder.buildFromScratch);
        var $20 = buildRecordArgs(dictBuildRecordArgs)(Type_Proxy["Proxy"].value);
        return function ($21) {
            return $19($20($21));
        };
    };
};
var buildArgsCons = function (dictIsSymbol) {
    var insert1 = insert(dictIsSymbol);
    var get = Record.get(dictIsSymbol)();
    return function () {
        return function () {
            return function () {
                return function (dictBuildRecordArgs) {
                    var buildRecordArgs1 = buildRecordArgs(dictBuildRecordArgs);
                    return {
                        buildRecordArgs: function (v) {
                            return function (rs) {
                                return apply(map(function (a) {
                                    return function (b) {
                                        return composeFlipped(insert1(Type_Proxy["Proxy"].value)(a))(b);
                                    };
                                })(get(Type_Proxy["Proxy"].value)(rs)))(buildRecordArgs1(Type_Proxy["Proxy"].value)(rs));
                            };
                        }
                    };
                };
            };
        };
    };
};
export {
    buildRecordArgs,
    fromRecord,
    buildRecordArgsNil,
    buildArgsCons
};
