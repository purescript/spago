// Generated by purs version 0.15.10
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Test_Spec_Result from "../Test.Spec.Result/index.js";
import * as Test_Spec_Tree from "../Test.Spec.Tree/index.js";
var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};
var semiringRecord = /* #__PURE__ */ Data_Semiring.semiringRecord()(/* #__PURE__ */ Data_Semiring.semiringRecordCons({
    reflectSymbol: function () {
        return "failed";
    }
})()(/* #__PURE__ */ Data_Semiring.semiringRecordCons({
    reflectSymbol: function () {
        return "passed";
    }
})()(/* #__PURE__ */ Data_Semiring.semiringRecordCons({
    reflectSymbol: function () {
        return "pending";
    }
})()(Data_Semiring.semiringRecordNil)(Data_Semiring.semiringInt))(Data_Semiring.semiringInt))(Data_Semiring.semiringInt));
var add = /* #__PURE__ */ Data_Semiring.add(semiringRecord);
var un = /* #__PURE__ */ Data_Newtype.un();
var Count = function (x) {
    return x;
};
var semigroupCount = {
    append: function (v) {
        return function (v1) {
            return add(v)(v1);
        };
    }
};
var newtypeSummary = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidCount = {
    mempty: /* #__PURE__ */ Data_Semiring.zero(semiringRecord),
    Semigroup0: function () {
        return semigroupCount;
    }
};
var $lazy_summarize = /* #__PURE__ */ $runtime_lazy("summarize", "Test.Spec.Summary", function () {
    return Data_Foldable.foldMap(Data_Foldable.foldableArray)(monoidCount)(function (v) {
        if (v instanceof Test_Spec_Tree.Leaf && (v.value1 instanceof Data_Maybe.Just && v.value1.value0 instanceof Test_Spec_Result.Success)) {
            return {
                passed: 1,
                failed: 0,
                pending: 0
            };
        };
        if (v instanceof Test_Spec_Tree.Leaf && (v.value1 instanceof Data_Maybe.Just && v.value1.value0 instanceof Test_Spec_Result.Failure)) {
            return {
                passed: 0,
                failed: 1,
                pending: 0
            };
        };
        if (v instanceof Test_Spec_Tree.Leaf && v.value1 instanceof Data_Maybe.Nothing) {
            return {
                passed: 0,
                failed: 0,
                pending: 1
            };
        };
        if (v instanceof Test_Spec_Tree.Node) {
            return $lazy_summarize(29)(v.value1);
        };
        throw new Error("Failed pattern match at Test.Spec.Summary (line 25, column 21 - line 29, column 32): " + [ v.constructor.name ]);
    });
});
var summarize = /* #__PURE__ */ $lazy_summarize(24);
var successful = function (groups) {
    return (un(Count)(summarize(groups))).failed === 0;
};
export {
    Count,
    summarize,
    successful,
    newtypeSummary,
    semigroupCount,
    monoidCount
};
//# sourceMappingURL=index.js.map
