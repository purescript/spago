// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var lmap = /* #__PURE__ */ Data_Bifunctor.lmap(Data_Bifunctor.bifunctorEither);
var applyFirst = /* #__PURE__ */ Control_Apply.applyFirst(Parsing.applyParserT);
var voidLeft = /* #__PURE__ */ Data_Functor.voidLeft(Parsing.functorParserT);
var printPosition = function (v) {
    return "(line " + (show(v.line) + (", col " + (show(v.column) + ")")));
};
var printError = function (err) {
    return Parsing.parseErrorMessage(err) + (" " + printPosition(Parsing.parseErrorPosition(err)));
};
var runP = function (p) {
    return function (s) {
        return lmap(printError)(Parsing.runParser(s)(applyFirst(p)(Parsing_String.eof)));
    };
};
var oneOfAs = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (dictFoldable) {
        var choice = Parsing_Combinators.choice(dictFoldable);
        return function (dictMonad) {
            return function (p) {
                return function (xs) {
                    return choice(map(function (v) {
                        return voidLeft(p(v.value0))(v.value1);
                    })(xs));
                };
            };
        };
    };
};
export {
    oneOfAs,
    runP
};
//# sourceMappingURL=index.js.map
