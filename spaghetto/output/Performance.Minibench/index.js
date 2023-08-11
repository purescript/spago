// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Number from "../Data.Number/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Effect_Ref from "../Effect.Ref/index.js";
var withUnits = function (t) {
    if (t < 1000.0) {
        return $foreign.toFixed(t) + " ns";
    };
    if (t < 1000000.0) {
        return $foreign.toFixed(t / 1000.0) + " \u03bcs";
    };
    if (t < 1.0e9) {
        return $foreign.toFixed(t / 1000000.0) + " ms";
    };
    if (Data_Boolean.otherwise) {
        return $foreign.toFixed(t / 1.0e9) + " s";
    };
    throw new Error("Failed pattern match at Performance.Minibench (line 33, column 1 - line 33, column 30): " + [ t.constructor.name ]);
};
var benchWith$prime = function (n) {
    return function (f) {
        return function __do() {
            var sumRef = Effect_Ref["new"](0.0)();
            var sum2Ref = Effect_Ref["new"](0.0)();
            var minRef = Effect_Ref["new"](Data_Number.infinity)();
            var maxRef = Effect_Ref["new"](0.0)();
            $foreign.gc();
            Effect.forE(0)(n)(function (v) {
                return function __do() {
                    var ns = $foreign.timeNs(f);
                    var square = ns * ns;
                    Effect_Ref.modify(function (v1) {
                        return v1 + ns;
                    })(sumRef)();
                    Effect_Ref.modify(function (v1) {
                        return v1 + square;
                    })(sum2Ref)();
                    Effect_Ref.modify(function (v1) {
                        return Data_Number.min(v1)(ns);
                    })(minRef)();
                    Effect_Ref.modify(function (v1) {
                        return Data_Number.max(v1)(ns);
                    })(maxRef)();
                    return Data_Unit.unit;
                };
            })();
            var sum = Effect_Ref.read(sumRef)();
            var sum2 = Effect_Ref.read(sum2Ref)();
            var min$prime = Effect_Ref.read(minRef)();
            var max$prime = Effect_Ref.read(maxRef)();
            var n$prime = Data_Int.toNumber(n);
            var mean = sum / n$prime;
            var stdDev = Data_Number.sqrt((sum2 - n$prime * mean * mean) / (n$prime - 1.0));
            return {
                mean: mean,
                stdDev: stdDev,
                min: min$prime,
                max: max$prime
            };
        };
    };
};
var benchWith = function (n) {
    return function (f) {
        return function __do() {
            var res = benchWith$prime(n)(f)();
            Effect_Console.log("mean   = " + withUnits(res.mean))();
            Effect_Console.log("stddev = " + withUnits(res.stdDev))();
            Effect_Console.log("min    = " + withUnits(res.min))();
            return Effect_Console.log("max    = " + withUnits(res.max))();
        };
    };
};
var bench = /* #__PURE__ */ benchWith(1000);
export {
    bench,
    benchWith,
    benchWith$prime,
    withUnits
};
//# sourceMappingURL=index.js.map
