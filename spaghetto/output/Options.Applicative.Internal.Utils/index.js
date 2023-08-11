// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_String_Regex from "../Data.String.Regex/index.js";
import * as Data_String_Regex_Flags from "../Data.String.Regex.Flags/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";
var eq = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Maybe.eqMaybe(Data_Eq.eqInt));
var whitespaceRegex = /* #__PURE__ */ (function () {
    var v = Data_String_Regex.regex("\\s+")(Data_String_Regex_Flags.noFlags);
    if (v instanceof Data_Either.Left) {
        return Partial_Unsafe.unsafeCrashWith("whitespaceRegex: `\\s+` seems to be invlaid, err: " + v.value0);
    };
    if (v instanceof Data_Either.Right) {
        return v.value0;
    };
    throw new Error("Failed pattern match at Options.Applicative.Internal.Utils (line 39, column 19 - line 41, column 15): " + [ v.constructor.name ]);
})();
var words = function (v) {
    if (v === "") {
        return [  ];
    };
    return Data_String_Regex.split(whitespaceRegex)(v);
};
var unWords = function (dictFoldable) {
    return Data_Foldable.intercalate(dictFoldable)(Data_Monoid.monoidString)(" ");
};
var unLines = function (dictFoldable) {
    return Data_Foldable.intercalate(dictFoldable)(Data_Monoid.monoidString)("\x0a");
};
var startsWith = function (p) {
    return function (s) {
        return eq(Data_String_CodePoints.indexOf(p)(s))(new Data_Maybe.Just(0));
    };
};
var lines = function (v) {
    if (v === "") {
        return [  ];
    };
    return Data_String_Common.split("\x0a")(v);
};
var apApplyFlipped = function (dictApply) {
    return Control_Apply.lift2(dictApply)(Data_Function.applyFlipped);
};
export {
    unLines,
    unWords,
    lines,
    words,
    whitespaceRegex,
    startsWith,
    apApplyFlipped
};
//# sourceMappingURL=index.js.map
