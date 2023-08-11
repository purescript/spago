// Generated by purs version 0.15.10
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
var HaveNothing = /* #__PURE__ */ (function () {
    function HaveNothing() {

    };
    HaveNothing.value = new HaveNothing();
    return HaveNothing;
})();
var HaveMetadata = /* #__PURE__ */ (function () {
    function HaveMetadata() {

    };
    HaveMetadata.value = new HaveMetadata();
    return HaveMetadata;
})();
var HaveCurrentData = /* #__PURE__ */ (function () {
    function HaveCurrentData() {

    };
    HaveCurrentData.value = new HaveCurrentData();
    return HaveCurrentData;
})();
var HaveFutureData = /* #__PURE__ */ (function () {
    function HaveFutureData() {

    };
    HaveFutureData.value = new HaveFutureData();
    return HaveFutureData;
})();
var HaveEnoughData = /* #__PURE__ */ (function () {
    function HaveEnoughData() {

    };
    HaveEnoughData.value = new HaveEnoughData();
    return HaveEnoughData;
})();
var toEnumReadyState = function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(HaveNothing.value);
    };
    if (v === 1) {
        return new Data_Maybe.Just(HaveMetadata.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(HaveCurrentData.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(HaveFutureData.value);
    };
    if (v === 4) {
        return new Data_Maybe.Just(HaveEnoughData.value);
    };
    return Data_Maybe.Nothing.value;
};
var showReadyState = {
    show: function (v) {
        if (v instanceof HaveNothing) {
            return "HaveNothing";
        };
        if (v instanceof HaveMetadata) {
            return "HaveMetadata";
        };
        if (v instanceof HaveCurrentData) {
            return "HaveCurrentData";
        };
        if (v instanceof HaveFutureData) {
            return "HaveFutureData";
        };
        if (v instanceof HaveEnoughData) {
            return "HaveEnoughData";
        };
        throw new Error("Failed pattern match at Web.HTML.HTMLMediaElement.ReadyState (line 31, column 1 - line 36, column 41): " + [ v.constructor.name ]);
    }
};
var fromEnumReadyState = function (v) {
    if (v instanceof HaveNothing) {
        return 0;
    };
    if (v instanceof HaveMetadata) {
        return 1;
    };
    if (v instanceof HaveCurrentData) {
        return 2;
    };
    if (v instanceof HaveFutureData) {
        return 3;
    };
    if (v instanceof HaveEnoughData) {
        return 4;
    };
    throw new Error("Failed pattern match at Web.HTML.HTMLMediaElement.ReadyState (line 50, column 3 - line 55, column 24): " + [ v.constructor.name ]);
};
var eqReadyState = {
    eq: function (x) {
        return function (y) {
            if (x instanceof HaveNothing && y instanceof HaveNothing) {
                return true;
            };
            if (x instanceof HaveMetadata && y instanceof HaveMetadata) {
                return true;
            };
            if (x instanceof HaveCurrentData && y instanceof HaveCurrentData) {
                return true;
            };
            if (x instanceof HaveFutureData && y instanceof HaveFutureData) {
                return true;
            };
            if (x instanceof HaveEnoughData && y instanceof HaveEnoughData) {
                return true;
            };
            return false;
        };
    }
};
var ordReadyState = {
    compare: function (x) {
        return function (y) {
            if (x instanceof HaveNothing && y instanceof HaveNothing) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof HaveNothing) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof HaveNothing) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof HaveMetadata && y instanceof HaveMetadata) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof HaveMetadata) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof HaveMetadata) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof HaveCurrentData && y instanceof HaveCurrentData) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof HaveCurrentData) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof HaveCurrentData) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof HaveFutureData && y instanceof HaveFutureData) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof HaveFutureData) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof HaveFutureData) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof HaveEnoughData && y instanceof HaveEnoughData) {
                return Data_Ordering.EQ.value;
            };
            throw new Error("Failed pattern match at Web.HTML.HTMLMediaElement.ReadyState (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqReadyState;
    }
};
var enumReadyState = {
    succ: /* #__PURE__ */ Data_Enum.defaultSucc(toEnumReadyState)(fromEnumReadyState),
    pred: /* #__PURE__ */ Data_Enum.defaultPred(toEnumReadyState)(fromEnumReadyState),
    Ord0: function () {
        return ordReadyState;
    }
};
var boundedReadyState = /* #__PURE__ */ (function () {
    return {
        bottom: HaveNothing.value,
        top: HaveEnoughData.value,
        Ord0: function () {
            return ordReadyState;
        }
    };
})();
var boundedEnumReadyState = {
    cardinality: 5,
    toEnum: toEnumReadyState,
    fromEnum: fromEnumReadyState,
    Bounded0: function () {
        return boundedReadyState;
    },
    Enum1: function () {
        return enumReadyState;
    }
};
export {
    HaveNothing,
    HaveMetadata,
    HaveCurrentData,
    HaveFutureData,
    HaveEnoughData,
    eqReadyState,
    ordReadyState,
    boundedReadyState,
    enumReadyState,
    boundedEnumReadyState,
    showReadyState
};
//# sourceMappingURL=index.js.map
