// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Lazy from "../Data.Lazy/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Lazy.functorLazy);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var pure = /* #__PURE__ */ Control_Applicative.pure(Data_Lazy.applicativeLazy);
var Yield = /* #__PURE__ */ (function () {
    function Yield(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Yield.create = function (value0) {
        return function (value1) {
            return new Yield(value0, value1);
        };
    };
    return Yield;
})();
var Skip = /* #__PURE__ */ (function () {
    function Skip(value0) {
        this.value0 = value0;
    };
    Skip.create = function (value0) {
        return new Skip(value0);
    };
    return Skip;
})();
var Done = /* #__PURE__ */ (function () {
    function Done() {

    };
    Done.value = new Done();
    return Done;
})();
var ListT = function (x) {
    return x;
};
var wrapLazy = function (dictApplicative) {
    var pure1 = Control_Applicative.pure(dictApplicative);
    return function (v) {
        return pure1(new Skip(v));
    };
};
var wrapEffect = function (dictFunctor) {
    var map2 = Data_Functor.map(dictFunctor);
    return function (v) {
        return map2(function ($332) {
            return Skip.create(Data_Lazy.defer(Data_Function["const"]($332)));
        })(v);
    };
};
var unfold = function (dictMonad) {
    var map2 = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
    return function (f) {
        return function (z) {
            var g = function (v) {
                if (v instanceof Data_Maybe.Just) {
                    return new Yield(v.value0.value1, Data_Lazy.defer(function (v1) {
                        return unfold(dictMonad)(f)(v.value0.value0);
                    }));
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 129, column 3 - line 129, column 60): " + [ v.constructor.name ]);
            };
            return map2(g)(f(z));
        };
    };
};
var uncons = function (dictMonad) {
    var pure1 = Control_Applicative.pure(dictMonad.Applicative0());
    var bind = Control_Bind.bind(dictMonad.Bind1());
    return function (v) {
        var g = function (v1) {
            if (v1 instanceof Yield) {
                return pure1(new Data_Maybe.Just(new Data_Tuple.Tuple(v1.value0, Data_Lazy.force(v1.value1))));
            };
            if (v1 instanceof Skip) {
                return uncons(dictMonad)(Data_Lazy.force(v1.value0));
            };
            if (v1 instanceof Done) {
                return pure1(Data_Maybe.Nothing.value);
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans (line 194, column 3 - line 194, column 50): " + [ v1.constructor.name ]);
        };
        return bind(v)(g);
    };
};
var tail = function (dictMonad) {
    var map2 = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
    var uncons1 = uncons(dictMonad);
    return function (l) {
        return map2(map(Data_Tuple.snd))(uncons1(l));
    };
};
var stepMap = function (dictFunctor) {
    var map2 = Data_Functor.map(dictFunctor);
    return function (f) {
        return function (v) {
            return map2(f)(v);
        };
    };
};
var takeWhile = function (dictApplicative) {
    var stepMap1 = stepMap((dictApplicative.Apply0()).Functor0());
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var $249 = f(v.value0);
                if ($249) {
                    return new Yield(v.value0, map1(takeWhile(dictApplicative)(f))(v.value1));
                };
                return Done.value;
            };
            if (v instanceof Skip) {
                return new Skip(map1(takeWhile(dictApplicative)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans (line 153, column 3 - line 153, column 68): " + [ v.constructor.name ]);
        };
        return stepMap1(g);
    };
};
var scanl = function (dictMonad) {
    var map2 = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
    var unfold1 = unfold(dictMonad);
    return function (f) {
        return function (b) {
            return function (l) {
                var g = function (v) {
                    var h = function (v1) {
                        if (v1 instanceof Yield) {
                            var b$prime$prime = f(v.value0)(v1.value0);
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(new Data_Tuple.Tuple(b$prime$prime, Data_Lazy.force(v1.value1)), v.value0));
                        };
                        if (v1 instanceof Skip) {
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0, Data_Lazy.force(v1.value0)), v.value0));
                        };
                        if (v1 instanceof Done) {
                            return Data_Maybe.Nothing.value;
                        };
                        throw new Error("Failed pattern match at Control.Monad.List.Trans (line 247, column 5 - line 247, column 78): " + [ v1.constructor.name ]);
                    };
                    return map2(h)(v.value1);
                };
                return unfold1(g)(new Data_Tuple.Tuple(b, l));
            };
        };
    };
};
var prepend$prime = function (dictApplicative) {
    var pure1 = Control_Applicative.pure(dictApplicative);
    return function (h) {
        return function (t) {
            return pure1(new Yield(h, t));
        };
    };
};
var prepend = function (dictApplicative) {
    var prepend$prime1 = prepend$prime(dictApplicative);
    return function (h) {
        return function (t) {
            return prepend$prime1(h)(Data_Lazy.defer(Data_Function["const"](t)));
        };
    };
};
var nil = function (dictApplicative) {
    return Control_Applicative.pure(dictApplicative)(Done.value);
};
var singleton = function (dictApplicative) {
    var prepend1 = prepend(dictApplicative);
    var nil1 = nil(dictApplicative);
    return function (a) {
        return prepend1(a)(nil1);
    };
};
var take = function (dictApplicative) {
    var nil1 = nil(dictApplicative);
    var stepMap1 = stepMap((dictApplicative.Apply0()).Functor0());
    return function (v) {
        return function (v1) {
            if (v === 0) {
                return nil1;
            };
            var f = function (v2) {
                if (v2 instanceof Yield) {
                    return new Yield(v2.value0, map1(take(dictApplicative)(v - 1 | 0))(v2.value1));
                };
                if (v2 instanceof Skip) {
                    return new Skip(map1(take(dictApplicative)(v))(v2.value0));
                };
                if (v2 instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 146, column 3 - line 146, column 47): " + [ v2.constructor.name ]);
            };
            return stepMap1(f)(v1);
        };
    };
};
var zipWith$prime = function (dictMonad) {
    var Applicative0 = dictMonad.Applicative0();
    var pure1 = Control_Applicative.pure(Applicative0);
    var nil1 = nil(Applicative0);
    var Bind1 = dictMonad.Bind1();
    var Functor0 = (Bind1.Apply0()).Functor0();
    var map2 = Data_Functor.map(Functor0);
    var prepend$prime1 = prepend$prime(Applicative0);
    var wrapEffect1 = wrapEffect(Functor0);
    var bind = Control_Bind.bind(Bind1);
    var uncons1 = uncons(dictMonad);
    return function (f) {
        var g = function (v) {
            return function (v1) {
                if (v1 instanceof Data_Maybe.Nothing) {
                    return pure1(nil1);
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return pure1(nil1);
                };
                if (v instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Just) {
                    return map2(Data_Function.flip(prepend$prime1)(Data_Lazy.defer(function (v2) {
                        return zipWith$prime(dictMonad)(f)(v.value0.value1)(v1.value0.value1);
                    })))(f(v.value0.value0)(v1.value0.value0));
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 259, column 3 - line 259, column 25): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        var loop = function (fa) {
            return function (fb) {
                return wrapEffect1(bind(uncons1(fa))(function (ua) {
                    return bind(uncons1(fb))(function (ub) {
                        return g(ua)(ub);
                    });
                }));
            };
        };
        return loop;
    };
};
var zipWith = function (dictMonad) {
    var pure1 = Control_Applicative.pure(dictMonad.Applicative0());
    var zipWith$prime1 = zipWith$prime(dictMonad);
    return function (f) {
        var g = function (a) {
            return function (b) {
                return pure1(f(a)(b));
            };
        };
        return zipWith$prime1(g);
    };
};
var newtypeListT = {
    Coercible0: function () {
        return undefined;
    }
};
var mapMaybe = function (dictFunctor) {
    var stepMap1 = stepMap(dictFunctor);
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                return Data_Maybe.fromMaybe(Skip.create)(map(Yield.create)(f(v.value0)))(map1(mapMaybe(dictFunctor)(f))(v.value1));
            };
            if (v instanceof Skip) {
                return new Skip(map1(mapMaybe(dictFunctor)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans (line 182, column 3 - line 182, column 72): " + [ v.constructor.name ]);
        };
        return stepMap1(g);
    };
};
var iterate = function (dictMonad) {
    var pure1 = Control_Applicative.pure(dictMonad.Applicative0());
    var unfold1 = unfold(dictMonad);
    return function (f) {
        return function (a) {
            var g = function (x) {
                return pure1(new Data_Maybe.Just(new Data_Tuple.Tuple(f(x), x)));
            };
            return unfold1(g)(a);
        };
    };
};
var repeat = function (dictMonad) {
    return iterate(dictMonad)(identity);
};
var head = function (dictMonad) {
    var map2 = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
    var uncons1 = uncons(dictMonad);
    return function (l) {
        return map2(map(Data_Tuple.fst))(uncons1(l));
    };
};
var functorListT = function (dictFunctor) {
    var stepMap1 = stepMap(dictFunctor);
    return {
        map: function (f) {
            var g = function (v) {
                if (v instanceof Yield) {
                    return new Yield(f(v.value0), map1(Data_Functor.map(functorListT(dictFunctor))(f))(v.value1));
                };
                if (v instanceof Skip) {
                    return new Skip(map1(Data_Functor.map(functorListT(dictFunctor))(f))(v.value0));
                };
                if (v instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 279, column 5 - line 279, column 48): " + [ v.constructor.name ]);
            };
            return stepMap1(g);
        }
    };
};
var fromEffect = function (dictApplicative) {
    var map2 = Data_Functor.map((dictApplicative.Apply0()).Functor0());
    var nil1 = nil(dictApplicative);
    return function (fa) {
        return map2(Data_Function.flip(Yield.create)(Data_Lazy.defer(function (v) {
            return nil1;
        })))(fa);
    };
};
var monadTransListT = {
    lift: function (dictMonad) {
        return fromEffect(dictMonad.Applicative0());
    }
};
var lift = /* #__PURE__ */ Control_Monad_Trans_Class.lift(monadTransListT);
var foldlRec$prime = function (dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var pure1 = Control_Applicative.pure(Monad0.Applicative0());
    var bind = Control_Bind.bind(Monad0.Bind1());
    var uncons1 = uncons(Monad0);
    var tailRecM2 = Control_Monad_Rec_Class.tailRecM2(dictMonadRec);
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return pure1(new Control_Monad_Rec_Class.Done(b));
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return bind(f(b)(v.value0.value0))(function (b$prime) {
                            return pure1(new Control_Monad_Rec_Class.Loop({
                                a: b$prime,
                                b: v.value0.value1
                            }));
                        });
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 220, column 5 - line 220, column 45): " + [ v.constructor.name ]);
                };
                return bind(uncons1(l))(g);
            };
        };
        return tailRecM2(loop);
    };
};
var runListTRec = function (dictMonadRec) {
    var pure1 = Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0());
    return foldlRec$prime(dictMonadRec)(function (v) {
        return function (v1) {
            return pure1(Data_Unit.unit);
        };
    })(Data_Unit.unit);
};
var foldlRec = function (dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var pure1 = Control_Applicative.pure(Monad0.Applicative0());
    var bind = Control_Bind.bind(Monad0.Bind1());
    var uncons1 = uncons(Monad0);
    var tailRecM2 = Control_Monad_Rec_Class.tailRecM2(dictMonadRec);
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return pure1(new Control_Monad_Rec_Class.Done(b));
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return pure1(new Control_Monad_Rec_Class.Loop({
                            a: f(b)(v.value0.value0),
                            b: v.value0.value1
                        }));
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 238, column 7 - line 238, column 47): " + [ v.constructor.name ]);
                };
                return bind(uncons1(l))(g);
            };
        };
        return tailRecM2(loop);
    };
};
var foldl$prime = function (dictMonad) {
    var pure1 = Control_Applicative.pure(dictMonad.Applicative0());
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var uncons1 = uncons(dictMonad);
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return pure1(b);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return bind(f(b)(v.value0.value0))(Data_Function.flip(loop)(v.value0.value1));
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 211, column 5 - line 211, column 35): " + [ v.constructor.name ]);
                };
                return bind(uncons1(l))(g);
            };
        };
        return loop;
    };
};
var runListT = function (dictMonad) {
    var pure1 = Control_Applicative.pure(dictMonad.Applicative0());
    return foldl$prime(dictMonad)(function (v) {
        return function (v1) {
            return pure1(Data_Unit.unit);
        };
    })(Data_Unit.unit);
};
var foldl = function (dictMonad) {
    var pure1 = Control_Applicative.pure(dictMonad.Applicative0());
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var uncons1 = uncons(dictMonad);
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return pure1(b);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return loop(f(b)(v.value0.value0))(v.value0.value1);
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 228, column 5 - line 228, column 35): " + [ v.constructor.name ]);
                };
                return bind(uncons1(l))(g);
            };
        };
        return loop;
    };
};
var filter = function (dictFunctor) {
    var stepMap1 = stepMap(dictFunctor);
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var s$prime = map1(filter(dictFunctor)(f))(v.value1);
                var $299 = f(v.value0);
                if ($299) {
                    return new Yield(v.value0, s$prime);
                };
                return new Skip(s$prime);
            };
            if (v instanceof Skip) {
                var s$prime = map1(filter(dictFunctor)(f))(v.value0);
                return new Skip(s$prime);
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans (line 175, column 3 - line 175, column 80): " + [ v.constructor.name ]);
        };
        return stepMap1(g);
    };
};
var dropWhile = function (dictApplicative) {
    var stepMap1 = stepMap((dictApplicative.Apply0()).Functor0());
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var $304 = f(v.value0);
                if ($304) {
                    return new Skip(map1(dropWhile(dictApplicative)(f))(v.value1));
                };
                return new Yield(v.value0, v.value1);
            };
            if (v instanceof Skip) {
                return new Skip(map1(dropWhile(dictApplicative)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans (line 168, column 3 - line 168, column 70): " + [ v.constructor.name ]);
        };
        return stepMap1(g);
    };
};
var drop = function (dictApplicative) {
    var stepMap1 = stepMap((dictApplicative.Apply0()).Functor0());
    return function (v) {
        return function (v1) {
            if (v === 0) {
                return v1;
            };
            var f = function (v2) {
                if (v2 instanceof Yield) {
                    return new Skip(map1(drop(dictApplicative)(v - 1 | 0))(v2.value1));
                };
                if (v2 instanceof Skip) {
                    return new Skip(map1(drop(dictApplicative)(v))(v2.value0));
                };
                if (v2 instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 161, column 3 - line 161, column 44): " + [ v2.constructor.name ]);
            };
            return stepMap1(f)(v1);
        };
    };
};
var cons = function (dictApplicative) {
    var pure1 = Control_Applicative.pure(dictApplicative);
    return function (lh) {
        return function (t) {
            return pure1(new Yield(Data_Lazy.force(lh), t));
        };
    };
};
var unfoldable1ListT = function (dictMonad) {
    var Applicative0 = dictMonad.Applicative0();
    var singleton1 = singleton(Applicative0);
    var cons1 = cons(Applicative0);
    return {
        unfoldr1: function (f) {
            return function (b) {
                var go = function (v) {
                    if (v.value1 instanceof Data_Maybe.Nothing) {
                        return singleton1(v.value0);
                    };
                    if (v.value1 instanceof Data_Maybe.Just) {
                        return cons1(pure(v.value0))(Data_Lazy.defer(function (v1) {
                            return go(f(v.value1.value0));
                        }));
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 293, column 12 - line 295, column 67): " + [ v.constructor.name ]);
                };
                return go(f(b));
            };
        }
    };
};
var unfoldableListT = function (dictMonad) {
    var Applicative0 = dictMonad.Applicative0();
    var nil1 = nil(Applicative0);
    var cons1 = cons(Applicative0);
    var unfoldable1ListT1 = unfoldable1ListT(dictMonad);
    return {
        unfoldr: function (f) {
            return function (b) {
                var go = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return nil1;
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return cons1(pure(v.value0.value0))(Data_Lazy.defer(function (v1) {
                            return go(f(v.value0.value1));
                        }));
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 286, column 12 - line 288, column 67): " + [ v.constructor.name ]);
                };
                return go(f(b));
            };
        },
        Unfoldable10: function () {
            return unfoldable1ListT1;
        }
    };
};
var semigroupListT = function (dictApplicative) {
    return {
        append: concat(dictApplicative)
    };
};
var concat = function (dictApplicative) {
    var stepMap1 = stepMap((dictApplicative.Apply0()).Functor0());
    return function (x) {
        return function (y) {
            var f = function (v) {
                if (v instanceof Yield) {
                    return new Yield(v.value0, map1(function (v1) {
                        return Data_Semigroup.append(semigroupListT(dictApplicative))(v1)(y);
                    })(v.value1));
                };
                if (v instanceof Skip) {
                    return new Skip(map1(function (v1) {
                        return Data_Semigroup.append(semigroupListT(dictApplicative))(v1)(y);
                    })(v.value0));
                };
                if (v instanceof Done) {
                    return new Skip(Data_Lazy.defer(Data_Function["const"](y)));
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 105, column 3 - line 105, column 43): " + [ v.constructor.name ]);
            };
            return stepMap1(f)(x);
        };
    };
};
var monoidListT = function (dictApplicative) {
    var semigroupListT1 = semigroupListT(dictApplicative);
    return {
        mempty: nil(dictApplicative),
        Semigroup0: function () {
            return semigroupListT1;
        }
    };
};
var catMaybes = function (dictFunctor) {
    return mapMaybe(dictFunctor)(identity);
};
var monadListT = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeListT(dictMonad);
        },
        Bind1: function () {
            return bindListT(dictMonad);
        }
    };
};
var bindListT = function (dictMonad) {
    var append = Data_Semigroup.append(semigroupListT(dictMonad.Applicative0()));
    var stepMap1 = stepMap(((dictMonad.Bind1()).Apply0()).Functor0());
    return {
        bind: function (fa) {
            return function (f) {
                var g = function (v) {
                    if (v instanceof Yield) {
                        var h = function (s$prime) {
                            return append(f(v.value0))(Control_Bind.bind(bindListT(dictMonad))(s$prime)(f));
                        };
                        return new Skip(map1(h)(v.value1));
                    };
                    if (v instanceof Skip) {
                        return new Skip(map1(function (v1) {
                            return Control_Bind.bind(bindListT(dictMonad))(v1)(f);
                        })(v.value0));
                    };
                    if (v instanceof Done) {
                        return Done.value;
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 305, column 5 - line 307, column 31): " + [ v.constructor.name ]);
                };
                return stepMap1(g)(fa);
            };
        },
        Apply0: function () {
            return applyListT(dictMonad);
        }
    };
};
var applyListT = function (dictMonad) {
    var functorListT1 = functorListT(((dictMonad.Bind1()).Apply0()).Functor0());
    return {
        apply: Control_Monad.ap(monadListT(dictMonad)),
        Functor0: function () {
            return functorListT1;
        }
    };
};
var applicativeListT = function (dictMonad) {
    return {
        pure: singleton(dictMonad.Applicative0()),
        Apply0: function () {
            return applyListT(dictMonad);
        }
    };
};
var monadEffectListT = function (dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var monadListT1 = monadListT(Monad0);
    return {
        liftEffect: (function () {
            var $333 = lift(Monad0);
            var $334 = Effect_Class.liftEffect(dictMonadEffect);
            return function ($335) {
                return $333($334($335));
            };
        })(),
        Monad0: function () {
            return monadListT1;
        }
    };
};
var altListT = function (dictApplicative) {
    var functorListT1 = functorListT((dictApplicative.Apply0()).Functor0());
    return {
        alt: concat(dictApplicative),
        Functor0: function () {
            return functorListT1;
        }
    };
};
var plusListT = function (dictMonad) {
    var Applicative0 = dictMonad.Applicative0();
    var altListT1 = altListT(Applicative0);
    return {
        empty: nil(Applicative0),
        Alt0: function () {
            return altListT1;
        }
    };
};
var alternativeListT = function (dictMonad) {
    var applicativeListT1 = applicativeListT(dictMonad);
    var plusListT1 = plusListT(dictMonad);
    return {
        Applicative0: function () {
            return applicativeListT1;
        },
        Plus1: function () {
            return plusListT1;
        }
    };
};
var monadPlusListT = function (dictMonad) {
    var monadListT1 = monadListT(dictMonad);
    var alternativeListT1 = alternativeListT(dictMonad);
    return {
        Monad0: function () {
            return monadListT1;
        },
        Alternative1: function () {
            return alternativeListT1;
        }
    };
};
export {
    ListT,
    Yield,
    Skip,
    Done,
    catMaybes,
    cons,
    drop,
    dropWhile,
    filter,
    foldl,
    foldlRec,
    foldl$prime,
    foldlRec$prime,
    fromEffect,
    head,
    iterate,
    mapMaybe,
    nil,
    prepend,
    prepend$prime,
    repeat,
    runListT,
    runListTRec,
    scanl,
    singleton,
    tail,
    take,
    takeWhile,
    uncons,
    unfold,
    wrapEffect,
    wrapLazy,
    zipWith,
    zipWith$prime,
    newtypeListT,
    semigroupListT,
    monoidListT,
    functorListT,
    unfoldableListT,
    unfoldable1ListT,
    applyListT,
    applicativeListT,
    bindListT,
    monadListT,
    monadTransListT,
    altListT,
    plusListT,
    alternativeListT,
    monadPlusListT,
    monadEffectListT
};
export {
    lift
} from "../Control.Monad.Trans.Class/index.js";
//# sourceMappingURL=index.js.map
