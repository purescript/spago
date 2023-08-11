// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Formatter_Internal from "../Data.Formatter.Internal/index.js";
import * as Data_Formatter_Parser_Number from "../Data.Formatter.Parser.Number/index.js";
import * as Data_Formatter_Parser_Utils from "../Data.Formatter.Parser.Utils/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Number from "../Data.Number/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var voidLeft = /* #__PURE__ */ Data_Functor.voidLeft(Parsing.functorParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var some = /* #__PURE__ */ Data_Array.some(Parsing.alternativeParserT)(Parsing.lazyParserT);
var parseDigit = /* #__PURE__ */ Data_Formatter_Parser_Number.parseDigit(Data_Identity.monadIdentity);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT);
var when = /* #__PURE__ */ Control_Applicative.when(Parsing.applicativeParserT);
var append = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var applyFirst = /* #__PURE__ */ Control_Apply.applyFirst(Parsing.applyParserT);
var foldDigits = /* #__PURE__ */ Data_Formatter_Internal.foldDigits(Data_Foldable.foldableArray);
var map = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var many = /* #__PURE__ */ Data_Array.many(Parsing.alternativeParserT)(Parsing.lazyParserT);
var repeat = /* #__PURE__ */ Data_Formatter_Internal.repeat(Data_Monoid.monoidString);
var $$for = /* #__PURE__ */ Data_Traversable["for"](Parsing.applicativeParserT)(Data_Traversable.traversableMaybe);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var bind1 = /* #__PURE__ */ Control_Bind.bind(Data_Either.bindEither);
var max = /* #__PURE__ */ Data_Ord.max(Data_Ord.ordInt);
var div1 = /* #__PURE__ */ Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt);
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var mapFlipped = /* #__PURE__ */ Data_Functor.mapFlipped(Data_Either.functorEither);
var show1 = /* #__PURE__ */ Data_Show.show(Data_Show.showNumber);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var Formatter = function (x) {
    return x;
};
var unformatParser = function (v) {
    return bind(Parsing_Combinators.optionMaybe(Parsing_Combinators["try"](Parsing_String.string("-"))))(function (minus) {
        return bind((function () {
            if (minus instanceof Data_Maybe.Nothing && v.sign) {
                return voidLeft(Parsing_String.string("+"))(1.0);
            };
            if (minus instanceof Data_Maybe.Nothing && Data_Boolean.otherwise) {
                return pure(1.0);
            };
            if (minus instanceof Data_Maybe.Just) {
                return pure(-1.0);
            };
            throw new Error("Failed pattern match at Data.Formatter.Number (line 169, column 11 - line 175, column 18): " + [ minus.constructor.name ]);
        })())(function (sign) {
            var digitsWithCommas$prime = function (accum) {
                return bind(some(parseDigit))(function (ds) {
                    return discard(when(Data_Array["null"](accum) && Data_Array.length(ds) > 3)(Parsing.fail("Wrong number of digits between thousand separators")))(function () {
                        return discard(when(Data_Array.length(ds) !== 3)(Parsing.fail("Wrong number of digits between thousand separators")))(function () {
                            return bind(Parsing_String_Basic.oneOf([ ",", "." ]))(function (sep) {
                                if (sep === ".") {
                                    return pure(append(accum)(ds));
                                };
                                if (sep === ",") {
                                    return digitsWithCommas$prime(append(accum)(ds));
                                };
                                return Parsing.fail("Incorrect symbol, expected ',' or '.'");
                            });
                        });
                    });
                });
            };
            var digitsWithCommas = (function () {
                var $93 = !v.comma;
                if ($93) {
                    return applyFirst(some(parseDigit))(Parsing_String.string("."));
                };
                return digitsWithCommas$prime([  ]);
            })();
            return bind(digitsWithCommas)(function (beforeDigits) {
                return bind((function () {
                    var $94 = Data_Array.length(beforeDigits) < v.before;
                    if ($94) {
                        return Parsing.fail("Error: too few digits before dot");
                    };
                    return pure(Data_Int.toNumber(foldDigits(beforeDigits)));
                })())(function (before) {
                    return bind(some(parseDigit))(function (afterDigits) {
                        return bind((function () {
                            var $95 = Data_Array.length(afterDigits) < v.after;
                            if ($95) {
                                return Parsing.fail("Error: too few digits after dot");
                            };
                            return pure(Data_Int.toNumber(foldDigits(afterDigits)));
                        })())(function (after) {
                            return bind((function () {
                                if (v.abbreviations) {
                                    return bind(Parsing_Combinators.optionMaybe(Parsing_Combinators["try"](Parsing_String_Basic.oneOf([ "K", "M", "G", "T", "P", "E", "Z", "Y" ]))))(function (letter) {
                                        if (letter instanceof Data_Maybe.Nothing) {
                                            return bind(Parsing_Combinators.optionMaybe(Parsing_String.string("10e+")))(function (e) {
                                                if (e instanceof Data_Maybe.Nothing) {
                                                    return pure(0);
                                                };
                                                if (e instanceof Data_Maybe.Just) {
                                                    return map(foldDigits)(many(parseDigit));
                                                };
                                                throw new Error("Failed pattern match at Data.Formatter.Number (line 221, column 11 - line 223, column 55): " + [ e.constructor.name ]);
                                            });
                                        };
                                        if (letter instanceof Data_Maybe.Just && letter.value0 === "K") {
                                            return pure(3);
                                        };
                                        if (letter instanceof Data_Maybe.Just && letter.value0 === "M") {
                                            return pure(6);
                                        };
                                        if (letter instanceof Data_Maybe.Just && letter.value0 === "G") {
                                            return pure(9);
                                        };
                                        if (letter instanceof Data_Maybe.Just && letter.value0 === "T") {
                                            return pure(12);
                                        };
                                        if (letter instanceof Data_Maybe.Just && letter.value0 === "P") {
                                            return pure(15);
                                        };
                                        if (letter instanceof Data_Maybe.Just && letter.value0 === "E") {
                                            return pure(18);
                                        };
                                        if (letter instanceof Data_Maybe.Just && letter.value0 === "Z") {
                                            return pure(21);
                                        };
                                        if (letter instanceof Data_Maybe.Just && letter.value0 === "Y") {
                                            return pure(24);
                                        };
                                        return pure(0);
                                    });
                                };
                                return pure(0);
                            })())(function (abbr) {
                                return pure(Data_Number.pow(10.0)(Data_Int.toNumber(abbr)) * sign * (before + after / Data_Number.pow(10.0)(Data_Int.toNumber(v.after))));
                            });
                        });
                    });
                });
            });
        });
    });
};
var unformat = function ($131) {
    return Data_Formatter_Parser_Utils.runP(unformatParser($131));
};
var printFormatter = function (v) {
    return (function () {
        if (v.sign) {
            return "+";
        };
        return "";
    })() + (repeat("0")(v.before - 1 | 0) + ((function () {
        if (v.comma) {
            return "0,0";
        };
        return "0";
    })() + ((function () {
        var $111 = v.after > 0;
        if ($111) {
            return ".";
        };
        return "";
    })() + (repeat("0")(v.after) + (function () {
        if (v.abbreviations) {
            return "a";
        };
        return "";
    })()))));
};
var newtypeFormatter = {
    Coercible0: function () {
        return undefined;
    }
};
var genericFormatter = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showFormatter = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericFormatter)(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(/* #__PURE__ */ Data_Show.showRecord()()(/* #__PURE__ */ Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "abbreviations";
        }
    })(/* #__PURE__ */ Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "after";
        }
    })(/* #__PURE__ */ Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "before";
        }
    })(/* #__PURE__ */ Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "comma";
        }
    })(/* #__PURE__ */ Data_Show.showRecordFieldsConsNil({
        reflectSymbol: function () {
            return "sign";
        }
    })(Data_Show.showBoolean))(Data_Show.showBoolean))(Data_Show.showInt))(Data_Show.showInt))(Data_Show.showBoolean))))({
        reflectSymbol: function () {
            return "Formatter";
        }
    }))
};
var formatParser = /* #__PURE__ */ bind(/* #__PURE__ */ Parsing_Combinators.optionMaybe(/* #__PURE__ */ Parsing_Combinators["try"](/* #__PURE__ */ Parsing_String.string("+"))))(function (sign) {
    return bind(some(Parsing_String.string("0")))(function (before) {
        return bind(Parsing_Combinators.optionMaybe(Parsing_Combinators["try"](Parsing_String.string(",0"))))(function (comma) {
            return bind(Parsing_Combinators.optionMaybe(Parsing_Combinators["try"](Parsing_String.string("."))))(function (dot) {
                return bind($$for(dot)(function (v) {
                    return Parsing_Combinators["try"](many(Parsing_String.string("0")));
                }))(function (after) {
                    return bind(Parsing_Combinators.optionMaybe(Parsing_Combinators["try"](Parsing_String.string("a"))))(function (abbreviations) {
                        return pure({
                            sign: Data_Maybe.isJust(sign),
                            before: Data_Array.length(before),
                            comma: Data_Maybe.isJust(comma),
                            after: Data_Maybe.fromMaybe(0)(map1(Data_Array.length)(after)),
                            abbreviations: Data_Maybe.isJust(abbreviations)
                        });
                    });
                });
            });
        });
    });
});
var parseFormatString = /* #__PURE__ */ Data_Formatter_Parser_Utils.runP(formatParser);
var unformatNumber = function (pattern) {
    return function (str) {
        return bind1(parseFormatString(pattern))(Data_Function.flip(unformat)(str));
    };
};
var format = function (v) {
    return function (num) {
        var absed = Data_Number.abs(num);
        var tens = (function () {
            if (absed > 0.0) {
                return max(Data_Int.floor(Data_Number.log(absed) / Data_Number.ln10))(0);
            };
            if (Data_Boolean.otherwise) {
                return 0;
            };
            throw new Error("Failed pattern match at Data.Formatter.Number (line 100, column 5 - line 102, column 22): " + [  ]);
        })();
        if (v.abbreviations) {
            var thousands = div1(tens)(3);
            var newNum = (function () {
                var $118 = thousands < 1;
                if ($118) {
                    return num;
                };
                return num / Data_Number.pow(1000.0)(Data_Int.toNumber(thousands));
            })();
            var abbr = (function () {
                if (thousands === 0) {
                    return "";
                };
                if (thousands === 1) {
                    return "K";
                };
                if (thousands === 2) {
                    return "M";
                };
                if (thousands === 3) {
                    return "G";
                };
                if (thousands === 4) {
                    return "T";
                };
                if (thousands === 5) {
                    return "P";
                };
                if (thousands === 6) {
                    return "E";
                };
                if (thousands === 7) {
                    return "Z";
                };
                if (thousands === 8) {
                    return "Y";
                };
                if (Data_Boolean.otherwise) {
                    return "10e+" + show(thousands * 3 | 0);
                };
                throw new Error("Failed pattern match at Data.Formatter.Number (line 107, column 7 - line 117, column 53): " + [  ]);
            })();
            return format({
                comma: v.comma,
                before: v.before,
                after: v.after,
                abbreviations: false,
                sign: v.sign
            })(newNum) + abbr;
        };
        var zeros = (v.before - tens | 0) - 1 | 0;
        var factor = Data_Number.pow(10.0)(Data_Int.toNumber(max(0)(v.after)));
        var rounded = Data_Number.round(absed * factor) / factor;
        var integer = Data_Number.floor(rounded);
        var leftoverDecimal = rounded - integer;
        var leftover = Data_Number.round(leftoverDecimal * factor);
        var leftoverWithZeros = (function () {
            var leftoverString = $foreign.showNumberAsInt(leftover);
            var leftoverLength = Data_String_CodePoints.length(leftoverString);
            var zeros$prime = repeat("0")(v.after - leftoverLength | 0);
            return zeros$prime + leftoverString;
        })();
        var leftovers = (function () {
            var $119 = v.after < 1;
            if ($119) {
                return "";
            };
            return "." + ((function () {
                var $120 = leftover === 0.0;
                if ($120) {
                    return repeat("0")(v.after);
                };
                return "";
            })() + (function () {
                var $121 = leftover > 0.0;
                if ($121) {
                    return leftoverWithZeros;
                };
                return "";
            })());
        })();
        var addCommas = function ($copy_acc) {
            return function ($copy_counter) {
                return function ($copy_input) {
                    var $tco_var_acc = $copy_acc;
                    var $tco_var_counter = $copy_counter;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(acc, counter, input) {
                        var v1 = Data_Array.uncons(input);
                        if (v1 instanceof Data_Maybe.Nothing) {
                            $tco_done = true;
                            return Data_String_CodeUnits.fromCharArray(acc);
                        };
                        if (v1 instanceof Data_Maybe.Just && counter < 3) {
                            $tco_var_acc = Data_Array.cons(v1.value0.head)(acc);
                            $tco_var_counter = counter + 1 | 0;
                            $copy_input = v1.value0.tail;
                            return;
                        };
                        $tco_var_acc = Data_Array.cons(",")(acc);
                        $tco_var_counter = 0;
                        $copy_input = input;
                        return;
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_acc, $tco_var_counter, $copy_input);
                    };
                    return $tco_result;
                };
            };
        };
        var shownInt = (function () {
            if (v.comma) {
                return addCommas([  ])(0)(Data_Array.reverse(Data_String_CodeUnits.toCharArray(repeat("0")(zeros) + $foreign.showNumberAsInt(integer))));
            };
            return repeat("0")(zeros) + $foreign.showNumberAsInt(integer);
        })();
        return (function () {
            var $127 = num < 0;
            if ($127) {
                return "-";
            };
            var $128 = num > 0 && v.sign;
            if ($128) {
                return "+";
            };
            return "";
        })() + (shownInt + leftovers);
    };
};
var formatNumber = function (pattern) {
    return function (number) {
        return mapFlipped(parseFormatString(pattern))(Data_Function.flip(format)(number));
    };
};
var formatOrShowNumber = function (patter) {
    return function (number) {
        return Data_Either.either(Data_Function["const"](show1(number)))(identity)(formatNumber(patter)(number));
    };
};
var eqFormatter = {
    eq: function (x) {
        return function (y) {
            return x.abbreviations === y.abbreviations && x.after === y.after && x.before === y.before && x.comma === y.comma && x.sign === y.sign;
        };
    }
};
export {
    Formatter,
    printFormatter,
    parseFormatString,
    format,
    unformat,
    formatNumber,
    formatOrShowNumber,
    unformatNumber,
    genericFormatter,
    newtypeFormatter,
    showFormatter,
    eqFormatter
};
//# sourceMappingURL=index.js.map
