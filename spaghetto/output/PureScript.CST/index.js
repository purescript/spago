// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Lazy from "../Data.Lazy/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as PureScript_CST_Lexer from "../PureScript.CST.Lexer/index.js";
import * as PureScript_CST_Parser from "../PureScript.CST.Parser/index.js";
import * as PureScript_CST_Parser_Monad from "../PureScript.CST.Parser.Monad/index.js";
import * as PureScript_CST_Print from "../PureScript.CST.Print/index.js";
import * as PureScript_CST_Range from "../PureScript.CST.Range/index.js";
import * as PureScript_CST_Range_TokenList from "../PureScript.CST.Range.TokenList/index.js";
import * as Unsafe_Coerce from "../Unsafe.Coerce/index.js";
var foldMap = /* #__PURE__ */ Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Monoid.monoidString);
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var bind = /* #__PURE__ */ Control_Bind.bind(PureScript_CST_Parser_Monad.bindParser);
var pure = /* #__PURE__ */ Control_Applicative.pure(PureScript_CST_Parser_Monad.applicativeParser);
var ParseSucceeded = /* #__PURE__ */ (function () {
    function ParseSucceeded(value0) {
        this.value0 = value0;
    };
    ParseSucceeded.create = function (value0) {
        return new ParseSucceeded(value0);
    };
    return ParseSucceeded;
})();
var ParseSucceededWithErrors = /* #__PURE__ */ (function () {
    function ParseSucceededWithErrors(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ParseSucceededWithErrors.create = function (value0) {
        return function (value1) {
            return new ParseSucceededWithErrors(value0, value1);
        };
    };
    return ParseSucceededWithErrors;
})();
var ParseFailed = /* #__PURE__ */ (function () {
    function ParseFailed(value0) {
        this.value0 = value0;
    };
    ParseFailed.create = function (value0) {
        return new ParseFailed(value0);
    };
    return ParseFailed;
})();
var PartialModule = function (x) {
    return x;
};
var toRecoveredParserResult = function (v) {
    var v1 = function (v2) {
        if (v instanceof Data_Either.Right && Data_Boolean.otherwise) {
            return new ParseSucceeded(v.value0.value0);
        };
        if (v instanceof Data_Either.Left) {
            return new ParseFailed(v.value0);
        };
        throw new Error("Failed pattern match at PureScript.CST (line 41, column 1 - line 44, column 29): " + [ v.constructor.name ]);
    };
    if (v instanceof Data_Either.Right) {
        var $21 = Data_Array_NonEmpty.fromArray(v.value0.value1);
        if ($21 instanceof Data_Maybe.Just) {
            return new ParseSucceededWithErrors(v.value0.value0, $21.value0);
        };
        return v1(true);
    };
    return v1(true);
};
var toRecovered = Unsafe_Coerce.unsafeCoerce;
var runRecoveredParser = function (p) {
    var $31 = Data_Function.flip(PureScript_CST_Parser_Monad.runParser)(p);
    return function ($32) {
        return toRecoveredParserResult($31(PureScript_CST_Lexer.lex($32)));
    };
};
var printModule = function (dictTokensOf) {
    var tokensOf = PureScript_CST_Range.tokensOf(PureScript_CST_Range.tokensOfModule(dictTokensOf));
    return function (mod) {
        return foldMap(PureScript_CST_Print.printSourceToken)(PureScript_CST_Range_TokenList.toArray(tokensOf(mod))) + foldMap(PureScript_CST_Print.printComment(PureScript_CST_Print.printLineFeed))((unwrap((unwrap(mod)).body)).trailingComments);
    };
};
var parseType = /* #__PURE__ */ runRecoveredParser(PureScript_CST_Parser.parseType);
var parsePartialModule = function (src) {
    return toRecoveredParserResult((function () {
        var v = PureScript_CST_Parser_Monad["runParser$prime"](PureScript_CST_Parser_Monad.initialParserState(PureScript_CST_Lexer.lex(src)))(PureScript_CST_Parser.parseModuleHeader);
        if (v instanceof PureScript_CST_Parser_Monad.ParseSucc) {
            var res = {
                header: v.value0,
                full: Data_Lazy.defer(function (v1) {
                    return toRecoveredParserResult(PureScript_CST_Parser_Monad.fromParserResult(PureScript_CST_Parser_Monad["runParser$prime"](v.value1)(bind(PureScript_CST_Parser.parseModuleBody)(function (body) {
                        return pure({
                            header: v.value0,
                            body: body
                        });
                    }))));
                })
            };
            return new Data_Either.Right(new Data_Tuple.Tuple(res, v.value1.errors));
        };
        if (v instanceof PureScript_CST_Parser_Monad.ParseFail) {
            return new Data_Either.Left(v.value0);
        };
        throw new Error("Failed pattern match at PureScript.CST (line 85, column 29 - line 97, column 17): " + [ v.constructor.name ]);
    })());
};
var parseModule = /* #__PURE__ */ runRecoveredParser(PureScript_CST_Parser.parseModule);
var parseImportDecl = /* #__PURE__ */ runRecoveredParser(PureScript_CST_Parser.parseImportDecl);
var parseExpr = /* #__PURE__ */ runRecoveredParser(PureScript_CST_Parser.parseExpr);
var parseDecl = /* #__PURE__ */ runRecoveredParser(PureScript_CST_Parser.parseDecl);
var parseBinder = /* #__PURE__ */ runRecoveredParser(PureScript_CST_Parser.parseBinder);
export {
    ParseSucceeded,
    ParseSucceededWithErrors,
    ParseFailed,
    PartialModule,
    parseModule,
    parsePartialModule,
    parseImportDecl,
    parseDecl,
    parseExpr,
    parseType,
    parseBinder,
    printModule,
    toRecovered
};
//# sourceMappingURL=index.js.map
