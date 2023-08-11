// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Formatter_DateTime from "../Data.Formatter.DateTime/index.js";
import * as Data_Formatter_Parser_Number from "../Data.Formatter.Parser.Number/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Interval from "../Data.Interval/index.js";
import * as Data_Interval_Duration from "../Data.Interval.Duration/index.js";
import * as Data_Interval_Duration_Iso from "../Data.Interval.Duration.Iso/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";
var apply = /* #__PURE__ */ Control_Apply.apply(Parsing.applyParserT);
var applyFirst = /* #__PURE__ */ Control_Apply.applyFirst(Parsing.applyParserT);
var map = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var choice = /* #__PURE__ */ Parsing_Combinators.choice(Data_Foldable.foldableArray);
var mapFlipped = /* #__PURE__ */ Data_Functor.mapFlipped(Data_Functor.functorArray);
var applySecond = /* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT);
var parseMaybeInteger = /* #__PURE__ */ Data_Formatter_Parser_Number.parseMaybeInteger(Data_Identity.monadIdentity);
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var fold = /* #__PURE__ */ Data_Foldable.fold(Data_Foldable.foldableMaybe);
var parseNumber = /* #__PURE__ */ Data_Formatter_Parser_Number.parseNumber(Data_Identity.monadIdentity);
var mapFlipped1 = /* #__PURE__ */ Data_Functor.mapFlipped(Parsing.functorParserT);
var sequence = /* #__PURE__ */ Data_Traversable.sequence(Data_Traversable.traversableArray)(Parsing.applicativeParserT);
var show = /* #__PURE__ */ Data_Show.show(/* #__PURE__ */ Data_Show.showArray(Data_Show.showString));
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Data_Interval_Duration.monoidDuration);
var intercalate = /* #__PURE__ */ Data_Foldable.intercalate(Data_List_Types.foldableNonEmptyList)(Data_Monoid.monoidString);
var map2 = /* #__PURE__ */ Data_Functor.map(Data_List_Types.functorNonEmptyList);
var parseInterval = function (duration) {
    return function (date) {
        var startEnd = apply(applyFirst(map(Data_Interval.StartEnd.create)(date))(Parsing_String.string("/")))(date);
        var startDuration = apply(applyFirst(map(Data_Interval.StartDuration.create)(date))(Parsing_String.string("/")))(duration);
        var durationOnly = map(Data_Interval.DurationOnly.create)(duration);
        var durationEnd = apply(applyFirst(map(Data_Interval.DurationEnd.create)(duration))(Parsing_String.string("/")))(date);
        return choice(mapFlipped([ startEnd, durationEnd, startDuration, durationOnly ])(Parsing_Combinators["try"]));
    };
};
var parseRecurringInterval = function (duration) {
    return function (date) {
        return apply(map(Data_Interval.RecurringInterval.create)(applySecond(Parsing_String.string("R"))(parseMaybeInteger)))(applySecond(Parsing_String.string("/"))(parseInterval(duration)(date)));
    };
};
var failIfEmpty = function (dictMonoid) {
    var mempty1 = Data_Monoid.mempty(dictMonoid);
    return function (dictEq) {
        var eq = Data_Eq.eq(dictEq);
        return function (p) {
            return function (str) {
                return bind(p)(function (x) {
                    var $42 = eq(x)(mempty1);
                    if ($42) {
                        return Parsing.fail(str);
                    };
                    return pure(x);
                });
            };
        };
    };
};
var failIfEmpty1 = /* #__PURE__ */ failIfEmpty(Data_Interval_Duration.monoidDuration)(Data_Interval_Duration.eqDuration);
var mkComponentsParser = function (arr) {
    var foldFoldableMaybe = function (dictFoldable) {
        var foldMap = Data_Foldable.foldMap(dictFoldable);
        return function (dictMonoid) {
            return foldMap(dictMonoid)(fold(dictMonoid));
        };
    };
    var component = function (designator) {
        return applyFirst(parseNumber)(Parsing_String.string(designator));
    };
    var applyDurations = function (v) {
        return Parsing_Combinators.optionMaybe(Parsing_Combinators["try"](map(v.value0)(component(v.value1))));
    };
    var p = mapFlipped1(sequence(mapFlipped(arr)(applyDurations)))(foldFoldableMaybe(Data_Foldable.foldableArray)(Data_Interval_Duration.monoidDuration));
    return failIfEmpty1(p)("None of valid duration components (" + (show(map1(Data_Tuple.snd)(arr)) + ") were present"));
};
var parseDuration = /* #__PURE__ */ (function () {
    var weekDuration = mkComponentsParser([ new Data_Tuple.Tuple(Data_Interval_Duration.week, "W") ]);
    var durationTimePart = Parsing_Combinators.option(mempty)(applySecond(Parsing_Combinators["try"](Parsing_String.string("T")))(mkComponentsParser([ new Data_Tuple.Tuple(Data_Interval_Duration.hour, "H"), new Data_Tuple.Tuple(Data_Interval_Duration.minute, "M"), new Data_Tuple.Tuple(Data_Interval_Duration.second, "S") ])));
    var durationDatePart = Parsing_Combinators.option(mempty)(Parsing_Combinators["try"](mkComponentsParser([ new Data_Tuple.Tuple(Data_Interval_Duration.year, "Y"), new Data_Tuple.Tuple(Data_Interval_Duration.month, "M"), new Data_Tuple.Tuple(Data_Interval_Duration.day, "D") ])));
    var fullDuration = failIfEmpty1(apply(map(Data_Semigroup.append(Data_Interval_Duration.semigroupDuration))(durationDatePart))(durationTimePart))("Must contain valid duration components");
    return applySecond(Parsing_String.string("P"))(Control_Alt.alt(Parsing.altParserT)(weekDuration)(fullDuration));
})();
var parseIsoDuration = /* #__PURE__ */ bind(parseDuration)(function (dur) {
    var v = Data_Interval_Duration_Iso.mkIsoDuration(dur);
    if (v instanceof Data_Either.Left) {
        var errorStr = intercalate(", ")(map2(Data_Interval_Duration_Iso.prettyError)(v.value0));
        return Parsing.fail("Extracted Duration is not valid ISO duration (" + (errorStr + ")"));
    };
    if (v instanceof Data_Either.Right) {
        return pure(v.value0);
    };
    throw new Error("Failed pattern match at Data.Formatter.Parser.Interval (line 42, column 3 - line 46, column 22): " + [ v.constructor.name ]);
});
var extendedDateTimeFormatInUTC = /* #__PURE__ */ Data_Either.either(Partial_Unsafe.unsafeCrashWith)(/* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn))(/* #__PURE__ */ Data_Formatter_DateTime.parseFormatString("YYYY-MM-DDTHH:mm:ssZ"));
var parseDateTime = function (dictMonad) {
    return Data_Formatter_DateTime.unformatParser(dictMonad)(extendedDateTimeFormatInUTC);
};
export {
    parseRecurringInterval,
    parseInterval,
    parseIsoDuration,
    parseDateTime,
    extendedDateTimeFormatInUTC
};
//# sourceMappingURL=index.js.map
