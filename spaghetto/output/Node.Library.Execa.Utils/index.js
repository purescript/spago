// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Effect from "../Effect/index.js";
import * as Node_Buffer_Immutable from "../Node.Buffer.Immutable/index.js";
import * as Node_Encoding from "../Node.Encoding/index.js";
import * as Record from "../Record/index.js";
var voidRight = /* #__PURE__ */ Data_Functor.voidRight(Effect.functorEffect);
var CustomError = function (x) {
    return x;
};
var utf8 = /* #__PURE__ */ (function () {
    var toString = Node_Buffer_Immutable.toString(Node_Encoding.UTF8.value);
    var toBuffer = Data_Function.flip(Node_Buffer_Immutable.fromString)(Node_Encoding.UTF8.value);
    return {
        toString: toString,
        toBuffer: toBuffer
    };
})();
var toError = function (v) {
    return v;
};
var getErrorOption = function (dictIsSymbol) {
    var get = Record.get(dictIsSymbol)();
    return function () {
        return function () {
            return function () {
                return function (sym) {
                    return function (v) {
                        return get(sym)(v);
                    };
                };
            };
        };
    };
};
var buildCustomError = function () {
    return function () {
        return function (msg) {
            return function (info) {
                return $foreign.buildCustomErrorImpl(msg, info);
            };
        };
    };
};
var bracketEffect = function (open) {
    return function (close) {
        return function (use) {
            return function __do() {
                var resource = open();
                var b = use(resource)();
                return voidRight(b)(close(resource))();
            };
        };
    };
};
export {
    buildCustomErrorImpl,
    newPassThroughStream
} from "./foreign.js";
export {
    utf8,
    CustomError,
    toError,
    getErrorOption,
    buildCustomError,
    bracketEffect
};
//# sourceMappingURL=index.js.map
