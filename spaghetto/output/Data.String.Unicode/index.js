// Generated by purs version 0.15.10
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_CodePoint_Unicode from "../Data.CodePoint.Unicode/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
var bindFlipped = /* #__PURE__ */ Control_Bind.bindFlipped(Control_Bind.bindArray);
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var convertFull = function (f) {
    var $4 = bindFlipped(f);
    return function ($5) {
        return Data_String_CodePoints.fromCodePointArray($4(Data_String_CodePoints.toCodePointArray($5)));
    };
};
var toLower = /* #__PURE__ */ convertFull(Data_CodePoint_Unicode.toLower);
var toUpper = /* #__PURE__ */ convertFull(Data_CodePoint_Unicode.toUpper);
var convert = function (f) {
    var $6 = map(f);
    return function ($7) {
        return Data_String_CodePoints.fromCodePointArray($6(Data_String_CodePoints.toCodePointArray($7)));
    };
};
var toLowerSimple = /* #__PURE__ */ convert(Data_CodePoint_Unicode.toLowerSimple);
var toUpperSimple = /* #__PURE__ */ convert(Data_CodePoint_Unicode.toUpperSimple);
var caseFoldSimple = /* #__PURE__ */ convert(Data_CodePoint_Unicode.caseFoldSimple);
var caseFold = /* #__PURE__ */ convertFull(Data_CodePoint_Unicode.caseFold);
var caselessMatch = function (s1) {
    return function (s2) {
        return caseFold(s1) === caseFold(s2);
    };
};
export {
    toUpper,
    toLower,
    caseFold,
    caselessMatch,
    toUpperSimple,
    toLowerSimple,
    caseFoldSimple
};
//# sourceMappingURL=index.js.map
