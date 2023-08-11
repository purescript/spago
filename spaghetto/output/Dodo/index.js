// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_String_Regex from "../Data.String.Regex/index.js";
import * as Data_String_Regex_Flags from "../Data.String.Regex.Flags/index.js";
import * as Data_String_Regex_Unsafe from "../Data.String.Regex.Unsafe/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Dodo_Internal from "../Dodo.Internal/index.js";
import * as Dodo_Internal_Buffer from "../Dodo.Internal.Buffer/index.js";
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Dodo_Internal.monoidDoc);
var append1 = /* #__PURE__ */ Data_Semigroup.append(Dodo_Internal.semigroupDoc);
var max = /* #__PURE__ */ Data_Ord.max(Data_Ord.ordInt);
var max1 = /* #__PURE__ */ Data_Ord.max(Data_Ord.ordNumber);
var min = /* #__PURE__ */ Data_Ord.min(Data_Ord.ordNumber);
var power = /* #__PURE__ */ Data_Monoid.power(Data_Monoid.monoidString);
var pure = /* #__PURE__ */ Control_Applicative.pure(Data_List_Types.applicativeList);
var Printer = function (x) {
    return x;
};
var Doc = /* #__PURE__ */ (function () {
    function Doc(value0) {
        this.value0 = value0;
    };
    Doc.create = function (value0) {
        return new Doc(value0);
    };
    return Doc;
})();
var Dedent = /* #__PURE__ */ (function () {
    function Dedent(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Dedent.create = function (value0) {
        return function (value1) {
            return new Dedent(value0, value1);
        };
    };
    return Dedent;
})();
var LeaveAnnotation = /* #__PURE__ */ (function () {
    function LeaveAnnotation(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    LeaveAnnotation.create = function (value0) {
        return function (value1) {
            return new LeaveAnnotation(value0, value1);
        };
    };
    return LeaveAnnotation;
})();
var LeaveFlexGroup = /* #__PURE__ */ (function () {
    function LeaveFlexGroup(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    LeaveFlexGroup.create = function (value0) {
        return function (value1) {
            return new LeaveFlexGroup(value0, value1);
        };
    };
    return LeaveFlexGroup;
})();
var LeaveLocal = /* #__PURE__ */ (function () {
    function LeaveLocal(value0) {
        this.value0 = value0;
    };
    LeaveLocal.create = function (value0) {
        return new LeaveLocal(value0);
    };
    return LeaveLocal;
})();
var NoFlexGroup = /* #__PURE__ */ (function () {
    function NoFlexGroup() {

    };
    NoFlexGroup.value = new NoFlexGroup();
    return NoFlexGroup;
})();
var FlexGroupPending = /* #__PURE__ */ (function () {
    function FlexGroupPending() {

    };
    FlexGroupPending.value = new FlexGroupPending();
    return FlexGroupPending;
})();
var FlexGroupReset = /* #__PURE__ */ (function () {
    function FlexGroupReset(value0) {
        this.value0 = value0;
    };
    FlexGroupReset.create = function (value0) {
        return new FlexGroupReset(value0);
    };
    return FlexGroupReset;
})();
var withPosition = /* #__PURE__ */ (function () {
    return Dodo_Internal.WithPosition.create;
})();
var withLocalOptions = /* #__PURE__ */ (function () {
    return Dodo_Internal.Local.create;
})();
var twoSpaces = {
    pageWidth: 80,
    ribbonRatio: 1.0,
    indentUnit: "  ",
    indentWidth: 2
};
var text = function (v) {
    if (v === "") {
        return Dodo_Internal.Empty.value;
    };
    return new Dodo_Internal.Text(Data_String_CodePoints.length(v), v);
};
var tabs = {
    pageWidth: 120,
    ribbonRatio: 1.0,
    indentUnit: "\x09",
    indentWidth: 4
};
var storeState = function (stack) {
    return function (v) {
        return {
            position: v.position,
            buffer: v.buffer,
            annotations: v.annotations,
            indentSpaces: v.indentSpaces,
            stack: stack,
            options: v.options
        };
    };
};
var space = /* #__PURE__ */ text(" ");
var resetState = function (v) {
    return {
        position: v.position,
        buffer: v.buffer,
        annotations: v.annotations,
        indentSpaces: v.indentSpaces,
        flexGroup: NoFlexGroup.value,
        options: v.options
    };
};
var plainText = {
    emptyBuffer: "",
    writeText: function (v) {
        return function (str) {
            return function (buff) {
                return buff + str;
            };
        };
    },
    writeIndent: function (v) {
        return function (str) {
            return function (buff) {
                return buff + str;
            };
        };
    },
    writeBreak: function (buff) {
        return buff + "\x0a";
    },
    enterAnnotation: function (v) {
        return function (v1) {
            return function (buff) {
                return buff;
            };
        };
    },
    leaveAnnotation: function (v) {
        return function (v1) {
            return function (buff) {
                return buff;
            };
        };
    },
    flushBuffer: function (buff) {
        return buff;
    }
};
var locally = function (k) {
    return function (doc) {
        return new Dodo_Internal.Local(function (options) {
            return new Data_Tuple.Tuple(k(options), doc);
        });
    };
};
var indent = /* #__PURE__ */ (function () {
    return Dodo_Internal.notEmpty(Dodo_Internal.Indent.create);
})();
var fourSpaces = {
    pageWidth: 120,
    ribbonRatio: 1.0,
    indentUnit: "    ",
    indentWidth: 4
};
var foldWith = function (dictFoldable) {
    var foldr = Data_Foldable.foldr(dictFoldable);
    return function (f) {
        return foldr(Dodo_Internal.bothNotEmpty(f))(mempty);
    };
};
var foldWithSeparator = function (dictFoldable) {
    var foldWith1 = foldWith(dictFoldable);
    return function (separator) {
        return foldWith1(function (a) {
            return function (b) {
                return append1(a)(append1(separator)(b));
            };
        });
    };
};
var flexSelect = function (doc1) {
    return function (doc2) {
        return function (doc3) {
            if (Dodo_Internal.isEmpty(doc1)) {
                return doc2;
            };
            if (Data_Boolean.otherwise) {
                return new Dodo_Internal.FlexSelect(doc1, doc2, doc3);
            };
            throw new Error("Failed pattern match at Dodo (line 89, column 1 - line 89, column 57): " + [ doc1.constructor.name, doc2.constructor.name, doc3.constructor.name ]);
        };
    };
};
var flexGroup = /* #__PURE__ */ Dodo_Internal.notEmpty(function (v) {
    if (v instanceof Dodo_Internal.FlexSelect && (Dodo_Internal.isEmpty(v.value1) && Dodo_Internal.isEmpty(v.value2))) {
        return v;
    };
    return new Dodo_Internal.FlexSelect(v, Dodo_Internal.Empty.value, Dodo_Internal.Empty.value);
});
var flexAlt = /* #__PURE__ */ (function () {
    return Dodo_Internal.FlexAlt.create;
})();
var encloseWithSeparator = function (dictFoldable) {
    var foldWithSeparator1 = foldWithSeparator(dictFoldable);
    return function (open) {
        return function (close) {
            return function (separator) {
                return function (inner) {
                    return append1(open)(append1(foldWithSeparator1(separator)(inner))(close));
                };
            };
        };
    };
};
var encloseEmptyAlt = function (open) {
    return function (close) {
        return function ($$default) {
            return function (inner) {
                if (Dodo_Internal.isEmpty(inner)) {
                    return $$default;
                };
                if (Data_Boolean.otherwise) {
                    return append1(open)(append1(inner)(close));
                };
                throw new Error("Failed pattern match at Dodo (line 176, column 1 - line 176, column 71): " + [ open.constructor.name, close.constructor.name, $$default.constructor.name, inner.constructor.name ]);
            };
        };
    };
};
var enclose = function (open) {
    return function (close) {
        return function (inner) {
            return append1(open)(append1(inner)(close));
        };
    };
};
var calcRibbonWidth = function (v) {
    return function (n) {
        return max(0)(Data_Int.ceil(v.ribbonRatio * Data_Int.toNumber(v.pageWidth - n | 0)));
    };
};
var storeOptions = function (prevIndent) {
    return function (localOptions) {
        return function (state) {
            var newOptions = {
                indentUnit: localOptions.indentUnit,
                indentWidth: localOptions.indentWidth,
                pageWidth: localOptions.pageWidth,
                ribbonRatio: localOptions.ribbonRatio
            };
            return {
                position: {
                    line: state.position.line,
                    column: state.position.column,
                    indent: state.position.indent,
                    nextIndent: localOptions.indent,
                    pageWidth: newOptions.pageWidth,
                    ribbonWidth: calcRibbonWidth(newOptions)(prevIndent)
                },
                buffer: state.buffer,
                annotations: state.annotations,
                indentSpaces: localOptions.indentSpaces,
                flexGroup: state.flexGroup,
                options: newOptions
            };
        };
    };
};
var print = function (v) {
    return function (opts) {
        var initOptions = {
            pageWidth: opts.pageWidth,
            ribbonRatio: max1(0.0)(min(1.0)(opts.ribbonRatio)),
            indentUnit: opts.indentUnit,
            indentWidth: opts.indentWidth
        };
        var initState = {
            position: {
                line: 0,
                column: 0,
                indent: 0,
                nextIndent: 0,
                pageWidth: initOptions.pageWidth,
                ribbonWidth: calcRibbonWidth(initOptions)(0)
            },
            buffer: Dodo_Internal_Buffer["new"](v.emptyBuffer),
            annotations: Data_List_Types.Nil.value,
            indentSpaces: "",
            flexGroup: NoFlexGroup.value,
            options: initOptions
        };
        var go = function ($copy_stack) {
            return function ($copy_state) {
                var $tco_var_stack = $copy_stack;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(stack, state) {
                    if (stack instanceof Data_List_Types.Nil) {
                        $tco_done = true;
                        return v.flushBuffer(Dodo_Internal_Buffer.get(state.buffer));
                    };
                    if (stack instanceof Data_List_Types.Cons) {
                        if (stack.value0 instanceof Doc) {
                            if (stack.value0.value0 instanceof Dodo_Internal.Append) {
                                $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value0), new Data_List_Types.Cons(new Doc(stack.value0.value0.value1), stack.value1));
                                $copy_state = state;
                                return;
                            };
                            if (stack.value0.value0 instanceof Dodo_Internal.Text) {
                                if (state.position.column === 0 && state.position.indent > 0) {
                                    $tco_var_stack = stack;
                                    $copy_state = {
                                        position: {
                                            line: state.position.line,
                                            column: state.position.indent,
                                            indent: state.position.indent,
                                            nextIndent: state.position.nextIndent,
                                            pageWidth: state.position.pageWidth,
                                            ribbonWidth: state.position.ribbonWidth
                                        },
                                        buffer: Dodo_Internal_Buffer.modify(v.writeIndent(state.position.indent)(state.indentSpaces))(state.buffer),
                                        annotations: state.annotations,
                                        indentSpaces: state.indentSpaces,
                                        flexGroup: state.flexGroup,
                                        options: state.options
                                    };
                                    return;
                                };
                                if ((state.position.column + stack.value0.value0.value0 | 0) <= (state.position.indent + state.position.ribbonWidth | 0)) {
                                    $tco_var_stack = stack.value1;
                                    $copy_state = {
                                        position: {
                                            line: state.position.line,
                                            column: state.position.column + stack.value0.value0.value0 | 0,
                                            indent: state.position.indent,
                                            nextIndent: state.position.nextIndent,
                                            pageWidth: state.position.pageWidth,
                                            ribbonWidth: state.position.ribbonWidth
                                        },
                                        buffer: Dodo_Internal_Buffer.modify(v.writeText(stack.value0.value0.value0)(stack.value0.value0.value1))(state.buffer),
                                        annotations: state.annotations,
                                        indentSpaces: state.indentSpaces,
                                        flexGroup: state.flexGroup,
                                        options: state.options
                                    };
                                    return;
                                };
                                if (Data_Boolean.otherwise) {
                                    if (state.flexGroup instanceof FlexGroupReset) {
                                        $tco_var_stack = state.flexGroup.value0.stack;
                                        $copy_state = resetState(state.flexGroup.value0);
                                        return;
                                    };
                                    $tco_var_stack = stack.value1;
                                    $copy_state = {
                                        position: {
                                            line: state.position.line,
                                            column: state.position.column + stack.value0.value0.value0 | 0,
                                            indent: state.position.indent,
                                            nextIndent: state.position.nextIndent,
                                            pageWidth: state.position.pageWidth,
                                            ribbonWidth: state.position.ribbonWidth
                                        },
                                        buffer: Dodo_Internal_Buffer.modify(v.writeText(stack.value0.value0.value0)(stack.value0.value0.value1))(state.buffer),
                                        annotations: state.annotations,
                                        indentSpaces: state.indentSpaces,
                                        flexGroup: NoFlexGroup.value,
                                        options: state.options
                                    };
                                    return;
                                };
                            };
                            if (stack.value0.value0 instanceof Dodo_Internal.Break) {
                                if (state.flexGroup instanceof FlexGroupReset) {
                                    $tco_var_stack = state.flexGroup.value0.stack;
                                    $copy_state = resetState(state.flexGroup.value0);
                                    return;
                                };
                                $tco_var_stack = stack.value1;
                                $copy_state = {
                                    position: {
                                        line: state.position.line + 1 | 0,
                                        column: 0,
                                        indent: state.position.nextIndent,
                                        nextIndent: state.position.nextIndent,
                                        pageWidth: state.position.pageWidth,
                                        ribbonWidth: calcRibbonWidth(state.options)(state.position.nextIndent)
                                    },
                                    buffer: Dodo_Internal_Buffer.modify(v.writeBreak)(state.buffer),
                                    annotations: state.annotations,
                                    indentSpaces: state.indentSpaces,
                                    flexGroup: NoFlexGroup.value,
                                    options: state.options
                                };
                                return;
                            };
                            if (stack.value0.value0 instanceof Dodo_Internal.Indent) {
                                if (state.position.column === 0) {
                                    $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value0), new Data_List_Types.Cons(new Dedent(state.indentSpaces, state.position.nextIndent), stack.value1));
                                    $copy_state = {
                                        position: {
                                            line: state.position.line,
                                            column: state.position.column,
                                            indent: state.position.nextIndent + opts.indentWidth | 0,
                                            nextIndent: state.position.nextIndent + opts.indentWidth | 0,
                                            pageWidth: state.position.pageWidth,
                                            ribbonWidth: calcRibbonWidth(state.options)(state.position.nextIndent + opts.indentWidth | 0)
                                        },
                                        buffer: state.buffer,
                                        annotations: state.annotations,
                                        indentSpaces: state.indentSpaces + opts.indentUnit,
                                        flexGroup: state.flexGroup,
                                        options: state.options
                                    };
                                    return;
                                };
                                if (Data_Boolean.otherwise) {
                                    $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value0), new Data_List_Types.Cons(new Dedent(state.indentSpaces, state.position.nextIndent), stack.value1));
                                    $copy_state = {
                                        position: {
                                            line: state.position.line,
                                            column: state.position.column,
                                            indent: state.position.indent,
                                            nextIndent: state.position.nextIndent + opts.indentWidth | 0,
                                            pageWidth: state.position.pageWidth,
                                            ribbonWidth: state.position.ribbonWidth
                                        },
                                        buffer: state.buffer,
                                        annotations: state.annotations,
                                        indentSpaces: state.indentSpaces + opts.indentUnit,
                                        flexGroup: state.flexGroup,
                                        options: state.options
                                    };
                                    return;
                                };
                            };
                            if (stack.value0.value0 instanceof Dodo_Internal.Align) {
                                if (state.position.column === 0) {
                                    $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value1), new Data_List_Types.Cons(new Dedent(state.indentSpaces, state.position.nextIndent), stack.value1));
                                    $copy_state = {
                                        position: {
                                            line: state.position.line,
                                            column: state.position.column,
                                            indent: state.position.nextIndent + stack.value0.value0.value0 | 0,
                                            nextIndent: state.position.nextIndent + stack.value0.value0.value0 | 0,
                                            pageWidth: state.position.pageWidth,
                                            ribbonWidth: calcRibbonWidth(state.options)(state.position.nextIndent + stack.value0.value0.value0 | 0)
                                        },
                                        buffer: state.buffer,
                                        annotations: state.annotations,
                                        indentSpaces: state.indentSpaces + power(" ")(stack.value0.value0.value0),
                                        flexGroup: state.flexGroup,
                                        options: state.options
                                    };
                                    return;
                                };
                                if (Data_Boolean.otherwise) {
                                    $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value1), new Data_List_Types.Cons(new Dedent(state.indentSpaces, state.position.nextIndent), stack.value1));
                                    $copy_state = {
                                        position: {
                                            line: state.position.line,
                                            column: state.position.column,
                                            indent: state.position.indent,
                                            nextIndent: state.position.nextIndent + stack.value0.value0.value0 | 0,
                                            pageWidth: state.position.pageWidth,
                                            ribbonWidth: state.position.ribbonWidth
                                        },
                                        buffer: state.buffer,
                                        annotations: state.annotations,
                                        indentSpaces: state.indentSpaces + power(" ")(stack.value0.value0.value0),
                                        flexGroup: state.flexGroup,
                                        options: state.options
                                    };
                                    return;
                                };
                            };
                            if (stack.value0.value0 instanceof Dodo_Internal.FlexSelect) {
                                if (state.flexGroup instanceof NoFlexGroup) {
                                    $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value0), new Data_List_Types.Cons(new LeaveFlexGroup(stack.value0.value0.value1, stack.value0.value0.value2), stack.value1));
                                    $copy_state = {
                                        position: state.position,
                                        buffer: state.buffer,
                                        annotations: state.annotations,
                                        indentSpaces: state.indentSpaces,
                                        flexGroup: FlexGroupPending.value,
                                        options: state.options
                                    };
                                    return;
                                };
                                if (state.flexGroup instanceof FlexGroupPending && state.position.ribbonWidth > 0) {
                                    $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value0), new Data_List_Types.Cons(new Doc(stack.value0.value0.value1), stack.value1));
                                    $copy_state = {
                                        position: state.position,
                                        buffer: Dodo_Internal_Buffer.branch(state.buffer),
                                        annotations: state.annotations,
                                        indentSpaces: state.indentSpaces,
                                        flexGroup: new FlexGroupReset(storeState(stack)(state)),
                                        options: state.options
                                    };
                                    return;
                                };
                                $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value0), new Data_List_Types.Cons(new Doc(stack.value0.value0.value1), stack.value1));
                                $copy_state = state;
                                return;
                            };
                            if (stack.value0.value0 instanceof Dodo_Internal.FlexAlt) {
                                if (state.flexGroup instanceof FlexGroupReset) {
                                    $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value0), stack.value1);
                                    $copy_state = state;
                                    return;
                                };
                                if (state.flexGroup instanceof FlexGroupPending && state.position.ribbonWidth > 0) {
                                    $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value0), stack.value1);
                                    $copy_state = {
                                        position: state.position,
                                        buffer: Dodo_Internal_Buffer.branch(state.buffer),
                                        annotations: state.annotations,
                                        indentSpaces: state.indentSpaces,
                                        flexGroup: new FlexGroupReset(storeState(new Data_List_Types.Cons(new Doc(stack.value0.value0.value1), stack.value1))(state)),
                                        options: state.options
                                    };
                                    return;
                                };
                                $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value1), stack.value1);
                                $copy_state = state;
                                return;
                            };
                            if (stack.value0.value0 instanceof Dodo_Internal.WithPosition) {
                                if (state.position.column === 0 && state.position.nextIndent > 0) {
                                    $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value0({
                                        line: state.position.line,
                                        column: state.position.nextIndent,
                                        indent: state.position.indent,
                                        nextIndent: state.position.nextIndent,
                                        pageWidth: state.position.pageWidth,
                                        ribbonWidth: state.position.ribbonWidth
                                    })), stack.value1);
                                    $copy_state = state;
                                    return;
                                };
                                if (Data_Boolean.otherwise) {
                                    $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value0(state.position)), stack.value1);
                                    $copy_state = state;
                                    return;
                                };
                            };
                            if (stack.value0.value0 instanceof Dodo_Internal.Annotate) {
                                $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0.value1), new Data_List_Types.Cons(new LeaveAnnotation(stack.value0.value0.value0, state.annotations), stack.value1));
                                $copy_state = {
                                    position: state.position,
                                    buffer: Dodo_Internal_Buffer.modify(v.enterAnnotation(stack.value0.value0.value0)(state.annotations))(state.buffer),
                                    annotations: new Data_List_Types.Cons(stack.value0.value0.value0, state.annotations),
                                    indentSpaces: state.indentSpaces,
                                    flexGroup: state.flexGroup,
                                    options: state.options
                                };
                                return;
                            };
                            if (stack.value0.value0 instanceof Dodo_Internal.Local) {
                                var prevOptions = {
                                    indent: state.position.indent,
                                    indentSpaces: state.indentSpaces,
                                    indentUnit: state.options.indentUnit,
                                    indentWidth: state.options.indentWidth,
                                    pageWidth: state.options.pageWidth,
                                    ribbonRatio: state.options.ribbonRatio
                                };
                                var v1 = stack.value0.value0.value0(prevOptions);
                                $tco_var_stack = new Data_List_Types.Cons(new Doc(v1.value1), new Data_List_Types.Cons(new LeaveLocal(prevOptions), stack.value1));
                                $copy_state = storeOptions(state.position.indent)(v1.value0)(state);
                                return;
                            };
                            if (stack.value0.value0 instanceof Dodo_Internal.Empty) {
                                $tco_var_stack = stack.value1;
                                $copy_state = state;
                                return;
                            };
                            throw new Error("Failed pattern match at Dodo (line 365, column 18 - line 477, column 23): " + [ stack.value0.value0.constructor.name ]);
                        };
                        if (stack.value0 instanceof LeaveFlexGroup) {
                            if (state.flexGroup instanceof NoFlexGroup) {
                                $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value1), stack.value1);
                                $copy_state = {
                                    position: state.position,
                                    buffer: Dodo_Internal_Buffer.commit(state.buffer),
                                    annotations: state.annotations,
                                    indentSpaces: state.indentSpaces,
                                    flexGroup: state.flexGroup,
                                    options: state.options
                                };
                                return;
                            };
                            $tco_var_stack = new Data_List_Types.Cons(new Doc(stack.value0.value0), stack.value1);
                            $copy_state = {
                                position: state.position,
                                buffer: Dodo_Internal_Buffer.commit(state.buffer),
                                annotations: state.annotations,
                                indentSpaces: state.indentSpaces,
                                flexGroup: NoFlexGroup.value,
                                options: state.options
                            };
                            return;
                        };
                        if (stack.value0 instanceof Dedent) {
                            $tco_var_stack = stack.value1;
                            $copy_state = {
                                position: {
                                    line: state.position.line,
                                    column: state.position.column,
                                    indent: state.position.indent,
                                    nextIndent: stack.value0.value1,
                                    pageWidth: state.position.pageWidth,
                                    ribbonWidth: state.position.ribbonWidth
                                },
                                buffer: state.buffer,
                                annotations: state.annotations,
                                indentSpaces: stack.value0.value0,
                                flexGroup: state.flexGroup,
                                options: state.options
                            };
                            return;
                        };
                        if (stack.value0 instanceof LeaveAnnotation) {
                            $tco_var_stack = stack.value1;
                            $copy_state = {
                                position: state.position,
                                buffer: Dodo_Internal_Buffer.modify(v.leaveAnnotation(stack.value0.value0)(stack.value0.value1))(state.buffer),
                                annotations: stack.value0.value1,
                                indentSpaces: state.indentSpaces,
                                flexGroup: state.flexGroup,
                                options: state.options
                            };
                            return;
                        };
                        if (stack.value0 instanceof LeaveLocal) {
                            $tco_var_stack = stack.value1;
                            $copy_state = storeOptions(state.position.indent)(stack.value0.value0)(state);
                            return;
                        };
                        throw new Error("Failed pattern match at Dodo (line 364, column 18 - line 499, column 70): " + [ stack.value0.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Dodo (line 361, column 20 - line 499, column 70): " + [ stack.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_stack, $copy_state);
                };
                return $tco_result;
            };
        };
        var $135 = Data_Function.flip(go)(initState);
        return function ($136) {
            return $135(pure(Doc.create($136)));
        };
    };
};
var $$break = /* #__PURE__ */ (function () {
    return Dodo_Internal.Break.value;
})();
var softBreak = /* #__PURE__ */ flexAlt(mempty)($$break);
var spaceBreak = /* #__PURE__ */ flexAlt(space)($$break);
var appendSpaceBreak = /* #__PURE__ */ Dodo_Internal.bothNotEmpty(function (a) {
    return function (b) {
        return append1(a)(flexGroup(append1(spaceBreak)(b)));
    };
});
var paragraph = function (dictFoldable) {
    return Data_Foldable.foldl(dictFoldable)(appendSpaceBreak)(Dodo_Internal.Empty.value);
};
var textParagraph = /* #__PURE__ */ (function () {
    var spaceRegex = Data_String_Regex_Unsafe.unsafeRegex("[\\s\\n]+")(Data_String_Regex_Flags.global);
    var $137 = paragraph(Data_Foldable.foldableArray);
    var $138 = Data_Functor.map(Data_Functor.functorArray)(text);
    var $139 = Data_String_Regex.split(spaceRegex);
    return function ($140) {
        return $137($138($139(Data_String_Common.trim($140))));
    };
})();
var appendSpace = /* #__PURE__ */ Dodo_Internal.bothNotEmpty(function (a) {
    return function (b) {
        return append1(a)(append1(space)(b));
    };
});
var words = function (dictFoldable) {
    return Data_Foldable.foldr(dictFoldable)(appendSpace)(Dodo_Internal.Empty.value);
};
var appendBreak = /* #__PURE__ */ Dodo_Internal.bothNotEmpty(function (a) {
    return function (b) {
        return append1(a)(append1($$break)(b));
    };
});
var lines = function (dictFoldable) {
    return Data_Foldable.foldr(dictFoldable)(appendBreak)(Dodo_Internal.Empty.value);
};
var annotate = function ($141) {
    return Dodo_Internal.notEmpty(Dodo_Internal.Annotate.create($141));
};
var align = function (n) {
    return function (doc) {
        if (n > 0) {
            return Dodo_Internal.notEmpty(Dodo_Internal.Align.create(n))(doc);
        };
        if (Data_Boolean.otherwise) {
            return doc;
        };
        throw new Error("Failed pattern match at Dodo (line 64, column 1 - line 64, column 41): " + [ n.constructor.name, doc.constructor.name ]);
    };
};
var alignCurrentColumn = /* #__PURE__ */ Dodo_Internal.notEmpty(function (doc) {
    return withPosition(function (pos) {
        return align(pos.column - pos.nextIndent | 0)(doc);
    });
});
export {
    indent,
    align,
    alignCurrentColumn,
    annotate,
    withPosition,
    text,
    $$break as break,
    spaceBreak,
    softBreak,
    space,
    lines,
    words,
    appendBreak,
    appendSpace,
    appendSpaceBreak,
    flexAlt,
    flexGroup,
    flexSelect,
    paragraph,
    textParagraph,
    enclose,
    encloseEmptyAlt,
    encloseWithSeparator,
    foldWithSeparator,
    foldWith,
    locally,
    withLocalOptions,
    print,
    Printer,
    plainText,
    twoSpaces,
    fourSpaces,
    tabs
};
export {
    bothNotEmpty,
    isEmpty,
    notEmpty
} from "../Dodo.Internal/index.js";
//# sourceMappingURL=index.js.map
