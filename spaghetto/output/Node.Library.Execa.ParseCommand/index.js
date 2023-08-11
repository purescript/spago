// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_String_NonEmpty_Internal from "../Data.String.NonEmpty.Internal/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators_Array from "../Parsing.Combinators.Array/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
import * as Safe_Coerce from "../Safe.Coerce/index.js";
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var anyTill = /* #__PURE__ */ Parsing_String.anyTill(Data_Identity.monadIdentity);
var oneOf = /* #__PURE__ */ Data_Foldable.oneOf(Data_Foldable.foldableArray)(Parsing.plusParserT);
var map = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var applySecond = /* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT);
var fold1 = /* #__PURE__ */ Data_Array_NonEmpty.fold1(Data_Semigroup.semigroupString);
var voidRight = /* #__PURE__ */ Data_Functor.voidRight(Parsing.functorParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT);
var optional = /* #__PURE__ */ Data_Maybe.optional(Parsing.altParserT)(Parsing.applicativeParserT);
var EscapedSpace = /* #__PURE__ */ (function () {
    function EscapedSpace() {

    };
    EscapedSpace.value = new EscapedSpace();
    return EscapedSpace;
})();
var EscapedQuote = /* #__PURE__ */ (function () {
    function EscapedQuote() {

    };
    EscapedQuote.value = new EscapedQuote();
    return EscapedQuote;
})();
var RightBoundary = /* #__PURE__ */ (function () {
    function RightBoundary() {

    };
    RightBoundary.value = new RightBoundary();
    return RightBoundary;
})();
var DoubleQuote = /* #__PURE__ */ (function () {
    function DoubleQuote() {

    };
    DoubleQuote.value = new DoubleQuote();
    return DoubleQuote;
})();
var SingleQuote = /* #__PURE__ */ (function () {
    function SingleQuote() {

    };
    SingleQuote.value = new SingleQuote();
    return SingleQuote;
})();
var EscapedDoubleQuote = /* #__PURE__ */ (function () {
    function EscapedDoubleQuote() {

    };
    EscapedDoubleQuote.value = new EscapedDoubleQuote();
    return EscapedDoubleQuote;
})();
var EscapedSingleQuote = /* #__PURE__ */ (function () {
    function EscapedSingleQuote() {

    };
    EscapedSingleQuote.value = new EscapedSingleQuote();
    return EscapedSingleQuote;
})();
var eqQuoteType = {
    eq: function (x) {
        return function (y) {
            if (x instanceof DoubleQuote && y instanceof DoubleQuote) {
                return true;
            };
            if (x instanceof SingleQuote && y instanceof SingleQuote) {
                return true;
            };
            if (x instanceof EscapedDoubleQuote && y instanceof EscapedDoubleQuote) {
                return true;
            };
            if (x instanceof EscapedSingleQuote && y instanceof EscapedSingleQuote) {
                return true;
            };
            return false;
        };
    }
};
var parseCommand$prime$prime = function (command) {
    var parseArgEnd = function (acc) {
        return bind(anyTill(oneOf([ map((function () {
            var $41 = Data_Tuple.Tuple.create(true);
            return function ($42) {
                return $41(Data_String_CodeUnits.singleton($42));
            };
        })())(applySecond(Parsing_String.string("\\"))(Parsing_String.anyChar)), map((function () {
            var $43 = Data_Tuple.Tuple.create(false);
            return function ($44) {
                return $43(fold1($44));
            };
        })())(Parsing_Combinators_Array.many1(Parsing_String.string(" "))), voidRight(new Data_Tuple.Tuple(false, ""))(Parsing_String.eof) ])))(function (v) {
            if (v.value1.value0) {
                return parseArgEnd(acc + (v.value0 + v.value1.value1));
            };
            var v1 = Data_String_NonEmpty_Internal.fromString(acc + v.value0);
            if (v1 instanceof Data_Maybe.Just) {
                return pure(v1.value0);
            };
            if (v1 instanceof Data_Maybe.Nothing) {
                return Parsing.fail("Arg was empty");
            };
            throw new Error("Failed pattern match at Node.Library.Execa.ParseCommand (line 66, column 10 - line 68, column 38): " + [ v1.constructor.name ]);
        });
    };
    var parseArgPart = function (quoteStr) {
        return function (acc) {
            return bind(anyTill(oneOf([ voidRight(true)(Parsing_String.string("\\" + Data_String_NonEmpty_Internal.toString(quoteStr))), voidRight(false)(Parsing_String.string(Data_String_NonEmpty_Internal.toString(quoteStr))) ])))(function (v) {
                if (v.value1) {
                    return parseArgPart(quoteStr)(acc + (v.value0 + Data_String_NonEmpty_Internal.toString(quoteStr)));
                };
                return discard(Parsing_String_Basic.skipSpaces)(function () {
                    var v1 = Data_String_NonEmpty_Internal.fromString(acc + v.value0);
                    if (v1 instanceof Data_Maybe.Just) {
                        return pure(v1.value0);
                    };
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return Parsing.fail("Arg was empty");
                    };
                    throw new Error("Failed pattern match at Node.Library.Execa.ParseCommand (line 80, column 7 - line 82, column 40): " + [ v1.constructor.name ]);
                });
            });
        };
    };
    var parseArg = bind(optional(oneOf([ map(Data_String_NonEmpty_Internal.NonEmptyString)(Parsing_String.string("\"")), map(Data_String_NonEmpty_Internal.NonEmptyString)(Parsing_String.string("'")) ])))(function (mbStartBoundary) {
        if (mbStartBoundary instanceof Data_Maybe.Nothing) {
            return parseArgEnd("");
        };
        if (mbStartBoundary instanceof Data_Maybe.Just) {
            return parseArgPart(mbStartBoundary.value0)("");
        };
        throw new Error("Failed pattern match at Node.Library.Execa.ParseCommand (line 52, column 5 - line 56, column 33): " + [ mbStartBoundary.constructor.name ]);
    });
    var parseFileArgs = discard(Parsing_String_Basic.skipSpaces)(function () {
        return bind(parseArg)(function (file) {
            return bind(Parsing_Combinators_Array.many(parseArg))(function (args) {
                return pure({
                    file: file,
                    args: args
                });
            });
        });
    });
    return Parsing.runParser(command)(parseFileArgs);
};
var parseCommand$prime = /* #__PURE__ */ (function () {
    var dropNES = Safe_Coerce.coerce();
    return function ($45) {
        return dropNES(parseCommand$prime$prime($45));
    };
})();
var parseCommand = function ($46) {
    return Data_Either.hush(parseCommand$prime($46));
};
export {
    parseCommand,
    parseCommand$prime
};
//# sourceMappingURL=index.js.map
