// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_ST_Internal from "../Control.Monad.ST.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
var bind = /* #__PURE__ */ Control_Bind.bind(Control_Monad_ST_Internal.bindST);
var withArray = function (f) {
    return function (xs) {
        return function __do() {
            var result = $foreign.thaw(xs)();
            f(result)();
            return $foreign.unsafeFreeze(result)();
        };
    };
};
var unshift = function (a) {
    return $foreign.unshiftAll([ a ]);
};
var sortBy = function (comp) {
    return $foreign.sortByImpl(comp)(function (v) {
        if (v instanceof Data_Ordering.GT) {
            return 1;
        };
        if (v instanceof Data_Ordering.EQ) {
            return 0;
        };
        if (v instanceof Data_Ordering.LT) {
            return -1 | 0;
        };
        throw new Error("Failed pattern match at Data.Array.ST (line 109, column 31 - line 112, column 11): " + [ v.constructor.name ]);
    });
};
var sortWith = function (dictOrd) {
    var comparing = Data_Ord.comparing(dictOrd);
    return function (f) {
        return sortBy(comparing(f));
    };
};
var sort = function (dictOrd) {
    return sortBy(Data_Ord.compare(dictOrd));
};
var shift = /* #__PURE__ */ (function () {
    return $foreign.shiftImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var run = function (st) {
    return bind(st)($foreign.unsafeFreeze)();
};
var push = function (a) {
    return $foreign.pushAll([ a ]);
};
var pop = /* #__PURE__ */ (function () {
    return $foreign.popImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var peek = /* #__PURE__ */ (function () {
    return $foreign.peekImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var modify = function (i) {
    return function (f) {
        return function (xs) {
            return function __do() {
                var entry = peek(i)(xs)();
                if (entry instanceof Data_Maybe.Just) {
                    return $foreign.poke(i)(f(entry.value0))(xs)();
                };
                if (entry instanceof Data_Maybe.Nothing) {
                    return false;
                };
                throw new Error("Failed pattern match at Data.Array.ST (line 197, column 3 - line 199, column 26): " + [ entry.constructor.name ]);
            };
        };
    };
};
export {
    new,
    poke,
    length,
    pushAll,
    unshiftAll,
    splice,
    freeze,
    thaw,
    unsafeFreeze,
    unsafeThaw,
    toAssocArray
} from "./foreign.js";
export {
    run,
    withArray,
    peek,
    modify,
    pop,
    push,
    shift,
    unshift,
    sort,
    sortBy,
    sortWith
};
//# sourceMappingURL=index.js.map
