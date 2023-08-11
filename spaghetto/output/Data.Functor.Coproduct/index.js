// Generated by purs version 0.15.10
import * as Control_Comonad from "../Control.Comonad/index.js";
import * as Control_Extend from "../Control.Extend/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Show from "../Data.Show/index.js";
var bimap = /* #__PURE__ */ Data_Bifunctor.bimap(Data_Bifunctor.bifunctorEither);
var Coproduct = function (x) {
    return x;
};
var showCoproduct = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return function (dictShow1) {
        var show1 = Data_Show.show(dictShow1);
        return {
            show: function (v) {
                if (v instanceof Data_Either.Left) {
                    return "(left " + (show(v.value0) + ")");
                };
                if (v instanceof Data_Either.Right) {
                    return "(right " + (show1(v.value0) + ")");
                };
                throw new Error("Failed pattern match at Data.Functor.Coproduct (line 63, column 1 - line 65, column 60): " + [ v.constructor.name ]);
            }
        };
    };
};
var right = function (ga) {
    return new Data_Either.Right(ga);
};
var newtypeCoproduct = {
    Coercible0: function () {
        return undefined;
    }
};
var left = function (fa) {
    return new Data_Either.Left(fa);
};
var functorCoproduct = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (dictFunctor1) {
        var map1 = Data_Functor.map(dictFunctor1);
        return {
            map: function (f) {
                return function (v) {
                    return bimap(map(f))(map1(f))(v);
                };
            }
        };
    };
};
var eq1Coproduct = function (dictEq1) {
    var eq1 = Data_Eq.eq1(dictEq1);
    return function (dictEq11) {
        var eq11 = Data_Eq.eq1(dictEq11);
        return {
            eq1: function (dictEq) {
                var eq12 = eq1(dictEq);
                var eq13 = eq11(dictEq);
                return function (v) {
                    return function (v1) {
                        if (v instanceof Data_Either.Left && v1 instanceof Data_Either.Left) {
                            return eq12(v.value0)(v1.value0);
                        };
                        if (v instanceof Data_Either.Right && v1 instanceof Data_Either.Right) {
                            return eq13(v.value0)(v1.value0);
                        };
                        return false;
                    };
                };
            }
        };
    };
};
var eqCoproduct = function (dictEq1) {
    var eq1Coproduct1 = eq1Coproduct(dictEq1);
    return function (dictEq11) {
        var eq1 = Data_Eq.eq1(eq1Coproduct1(dictEq11));
        return function (dictEq) {
            return {
                eq: eq1(dictEq)
            };
        };
    };
};
var ord1Coproduct = function (dictOrd1) {
    var compare1 = Data_Ord.compare1(dictOrd1);
    var eq1Coproduct1 = eq1Coproduct(dictOrd1.Eq10());
    return function (dictOrd11) {
        var compare11 = Data_Ord.compare1(dictOrd11);
        var eq1Coproduct2 = eq1Coproduct1(dictOrd11.Eq10());
        return {
            compare1: function (dictOrd) {
                var compare12 = compare1(dictOrd);
                var compare13 = compare11(dictOrd);
                return function (v) {
                    return function (v1) {
                        if (v instanceof Data_Either.Left && v1 instanceof Data_Either.Left) {
                            return compare12(v.value0)(v1.value0);
                        };
                        if (v instanceof Data_Either.Left) {
                            return Data_Ordering.LT.value;
                        };
                        if (v1 instanceof Data_Either.Left) {
                            return Data_Ordering.GT.value;
                        };
                        if (v instanceof Data_Either.Right && v1 instanceof Data_Either.Right) {
                            return compare13(v.value0)(v1.value0);
                        };
                        throw new Error("Failed pattern match at Data.Functor.Coproduct (line 57, column 5 - line 61, column 43): " + [ v.constructor.name, v1.constructor.name ]);
                    };
                };
            },
            Eq10: function () {
                return eq1Coproduct2;
            }
        };
    };
};
var ordCoproduct = function (dictOrd1) {
    var ord1Coproduct1 = ord1Coproduct(dictOrd1);
    var eqCoproduct1 = eqCoproduct(dictOrd1.Eq10());
    return function (dictOrd11) {
        var compare1 = Data_Ord.compare1(ord1Coproduct1(dictOrd11));
        var eqCoproduct2 = eqCoproduct1(dictOrd11.Eq10());
        return function (dictOrd) {
            var eqCoproduct3 = eqCoproduct2(dictOrd.Eq0());
            return {
                compare: compare1(dictOrd),
                Eq0: function () {
                    return eqCoproduct3;
                }
            };
        };
    };
};
var coproduct = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Data_Either.Left) {
                return v(v2.value0);
            };
            if (v2 instanceof Data_Either.Right) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Functor.Coproduct (line 27, column 1 - line 27, column 78): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var extendCoproduct = function (dictExtend) {
    var extend = Control_Extend.extend(dictExtend);
    var functorCoproduct1 = functorCoproduct(dictExtend.Functor0());
    return function (dictExtend1) {
        var extend1 = Control_Extend.extend(dictExtend1);
        var functorCoproduct2 = functorCoproduct1(dictExtend1.Functor0());
        return {
            extend: function (f) {
                var $106 = coproduct((function () {
                    var $108 = extend(function ($110) {
                        return f(Coproduct(Data_Either.Left.create($110)));
                    });
                    return function ($109) {
                        return Data_Either.Left.create($108($109));
                    };
                })())((function () {
                    var $111 = extend1(function ($113) {
                        return f(Coproduct(Data_Either.Right.create($113)));
                    });
                    return function ($112) {
                        return Data_Either.Right.create($111($112));
                    };
                })());
                return function ($107) {
                    return Coproduct($106($107));
                };
            },
            Functor0: function () {
                return functorCoproduct2;
            }
        };
    };
};
var comonadCoproduct = function (dictComonad) {
    var extract = Control_Comonad.extract(dictComonad);
    var extendCoproduct1 = extendCoproduct(dictComonad.Extend0());
    return function (dictComonad1) {
        var extendCoproduct2 = extendCoproduct1(dictComonad1.Extend0());
        return {
            extract: coproduct(extract)(Control_Comonad.extract(dictComonad1)),
            Extend0: function () {
                return extendCoproduct2;
            }
        };
    };
};
var bihoistCoproduct = function (natF) {
    return function (natG) {
        return function (v) {
            return bimap(natF)(natG)(v);
        };
    };
};
export {
    Coproduct,
    left,
    right,
    coproduct,
    bihoistCoproduct,
    newtypeCoproduct,
    eqCoproduct,
    eq1Coproduct,
    ordCoproduct,
    ord1Coproduct,
    showCoproduct,
    functorCoproduct,
    extendCoproduct,
    comonadCoproduct
};
//# sourceMappingURL=index.js.map
