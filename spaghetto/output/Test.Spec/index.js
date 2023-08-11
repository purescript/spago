// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Fork_Class from "../Control.Monad.Fork.Class/index.js";
import * as Control_Monad_Writer_Class from "../Control.Monad.Writer.Class/index.js";
import * as Control_Monad_Writer_Trans from "../Control.Monad.Writer.Trans/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect_AVar from "../Effect.AVar/index.js";
import * as Effect_Aff_AVar from "../Effect.Aff.AVar/index.js";
import * as Effect_Aff_Class from "../Effect.Aff.Class/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
import * as Effect_Exception from "../Effect.Exception/index.js";
import * as Test_Spec_Tree from "../Test.Spec.Tree/index.js";
var over = /* #__PURE__ */ Data_Newtype.over()();
var alt = /* #__PURE__ */ Control_Alt.alt(Data_Maybe.altMaybe);
var monadTellWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadTellWriterT(Data_Monoid.monoidArray);
var monadThrowWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadThrowWriterT(Data_Monoid.monoidArray);
var monadStateWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadStateWriterT(Data_Monoid.monoidArray);
var monadWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadWriterT(Data_Monoid.monoidArray);
var monadRecWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadRecWriterT(Data_Monoid.monoidArray);
var monadReaderWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadReaderWriterT(Data_Monoid.monoidArray);
var monadPlusWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadPlusWriterT(Data_Monoid.monoidArray);
var monadErrorWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadErrorWriterT(Data_Monoid.monoidArray);
var monadEffectWriter1 = /* #__PURE__ */ Control_Monad_Writer_Trans.monadEffectWriter(Data_Monoid.monoidArray);
var monadContWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadContWriterT(Data_Monoid.monoidArray);
var monadAskWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadAskWriterT(Data_Monoid.monoidArray);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit);
var map = /* #__PURE__ */ Data_Functor.map(Data_Tuple.functorTuple);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var bimap = /* #__PURE__ */ Data_Bifunctor.bimap(Test_Spec_Tree.treeBifunctor);
var any = /* #__PURE__ */ Data_Foldable.any(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean);
var any1 = /* #__PURE__ */ Data_Foldable.any(Test_Spec_Tree.treeFoldable)(Data_HeytingAlgebra.heytingAlgebraBoolean);
var un = /* #__PURE__ */ Data_Newtype.un();
var map2 = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorFn);
var bindWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.bindWriterT(Data_Semigroup.semigroupArray);
var applyWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.applyWriterT(Data_Semigroup.semigroupArray);
var applicativeWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.applicativeWriterT(Data_Monoid.monoidArray);
var alternativeWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.alternativeWriterT(Data_Monoid.monoidArray);
var SpecT = function (x) {
    return x;
};
var MEmpty = /* #__PURE__ */ (function () {
    function MEmpty() {

    };
    MEmpty.value = new MEmpty();
    return MEmpty;
})();
var MMemoized = /* #__PURE__ */ (function () {
    function MMemoized(value0) {
        this.value0 = value0;
    };
    MMemoized.create = function (value0) {
        return new MMemoized(value0);
    };
    return MMemoized;
})();
var MFailed = /* #__PURE__ */ (function () {
    function MFailed(value0) {
        this.value0 = value0;
    };
    MFailed.create = function (value0) {
        return new MFailed(value0);
    };
    return MFailed;
})();
var CleanUpWithContext = /* #__PURE__ */ (function () {
    function CleanUpWithContext(value0) {
        this.value0 = value0;
    };
    CleanUpWithContext.create = function (value0) {
        return new CleanUpWithContext(value0);
    };
    return CleanUpWithContext;
})();
var TestWithName = /* #__PURE__ */ (function () {
    function TestWithName(value0) {
        this.value0 = value0;
    };
    TestWithName.create = function (value0) {
        return new TestWithName(value0);
    };
    return TestWithName;
})();
var warn = function () {
    return {};
};
var setParallelizable = function (value) {
    return over(Test_Spec_Tree.Item)(function (i) {
        return {
            isParallelizable: alt(i.isParallelizable)(new Data_Maybe.Just(value)),
            example: i.example,
            isFocused: i.isFocused
        };
    });
};
var plusSpecT = function (dictPlus) {
    return Control_Monad_Writer_Trans.plusWriterT(dictPlus);
};
var pending = function (dictMonad) {
    var tell = Control_Monad_Writer_Class.tell(monadTellWriterT(dictMonad));
    return function (name) {
        return tell([ new Test_Spec_Tree.Leaf(name, Data_Maybe.Nothing.value) ]);
    };
};
var pending$prime = function (dictMonad) {
    var pending1 = pending(dictMonad);
    return function (name) {
        return function (v) {
            return pending1(name);
        };
    };
};
var newtypeSpecT = {
    Coercible0: function () {
        return undefined;
    }
};
var monadTransSpecT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadTransWriterT(Data_Monoid.monoidArray);
var monadThrowSpecT = function (dictMonadThrow) {
    return monadThrowWriterT(dictMonadThrow);
};
var monadStateSpecT = function (dictMonadState) {
    return monadStateWriterT(dictMonadState);
};
var monadSpecT = function (dictMonad) {
    return monadWriterT(dictMonad);
};
var monadRecSpecT = function (dictMonadRec) {
    return monadRecWriterT(dictMonadRec);
};
var monadReaderSpecT = function (dictMonadReader) {
    return monadReaderWriterT(dictMonadReader);
};
var monadPlusSpecT = function (dictMonadPlus) {
    return monadPlusWriterT(dictMonadPlus);
};
var monadErrorSpecT = function (dictMonadError) {
    return monadErrorWriterT(dictMonadError);
};
var monadEffectWriter = function (dictMonadEffect) {
    return monadEffectWriter1(dictMonadEffect);
};
var monadContSpecT = function (dictMonadCont) {
    return monadContWriterT(dictMonadCont);
};
var monadAskSpecT = function (dictMonadAsk) {
    return monadAskWriterT(dictMonadAsk);
};
var memoize = function (dictMonadAff) {
    var Monad0 = (dictMonadAff.MonadEffect0()).Monad0();
    var Bind1 = Monad0.Bind1();
    var bind = Control_Bind.bind(Bind1);
    var liftAff = Effect_Aff_Class.liftAff(dictMonadAff);
    var applyFirst = Control_Apply.applyFirst(Bind1.Apply0());
    var pure = Control_Applicative.pure(Monad0.Applicative0());
    var discard1 = discard(Bind1);
    return function (dictMonadError) {
        var throwError = Control_Monad_Error_Class.throwError(dictMonadError.MonadThrow0());
        var $$try = Control_Monad_Error_Class["try"](dictMonadError);
        return function ($$var) {
            return function (action) {
                return bind(liftAff(Effect_Aff_AVar.take($$var)))(function (v) {
                    if (v instanceof MFailed) {
                        return throwError(Effect_Exception.error("exception in beforeAll-hook (see previous failure)"));
                    };
                    if (v instanceof MMemoized) {
                        return applyFirst(pure(v.value0))(liftAff(Effect_Aff_AVar.put(new MMemoized(v.value0))($$var)));
                    };
                    if (v instanceof MEmpty) {
                        return bind($$try(action))(function (res) {
                            return discard1(liftAff(Effect_Aff_AVar.put(Data_Either.either(MFailed.create)(MMemoized.create)(res))($$var)))(function () {
                                return Data_Either.either(throwError)(pure)(res);
                            });
                        });
                    };
                    throw new Error("Failed pattern match at Test.Spec (line 314, column 31 - line 320, column 33): " + [ v.constructor.name ]);
                });
            };
        };
    };
};
var mapSpecTree = function (dictFunctor) {
    var map3 = Data_Functor.map(dictFunctor);
    return function (g) {
        return function (f) {
            return over(SpecT)(Control_Monad_Writer_Trans.mapWriterT((function () {
                var $225 = map3(map(map1(f)));
                return function ($226) {
                    return $225(g($226));
                };
            })()));
        };
    };
};
var parallel = function (dictMonad) {
    return mapSpecTree(((dictMonad.Bind1()).Apply0()).Functor0())(identity)(bimap(identity)(setParallelizable(true)));
};
var sequential = function (dictMonad) {
    return mapSpecTree(((dictMonad.Bind1()).Apply0()).Functor0())(identity)(bimap(identity)(setParallelizable(false)));
};
var hoistSpec = function (dictMonad) {
    var mapSpecTree1 = mapSpecTree(((dictMonad.Bind1()).Apply0()).Functor0());
    return function (onM) {
        return function (f) {
            var onTest = function (name) {
                return over(Test_Spec_Tree.Item)(function (item) {
                    var e = function (g) {
                        return g((function () {
                            var $227 = f(new TestWithName(name));
                            return function ($228) {
                                return $227(item.example(Data_Function.applyFlipped($228)));
                            };
                        })());
                    };
                    return {
                        example: e,
                        isFocused: item.isFocused,
                        isParallelizable: item.isParallelizable
                    };
                });
            };
            var onCleanUp = function (name) {
                return function (around$prime) {
                    return function (i) {
                        return f(new CleanUpWithContext(name))(around$prime(i));
                    };
                };
            };
            return mapSpecTree1(onM)(Test_Spec_Tree.bimapTree(onCleanUp)(onTest));
        };
    };
};
var functorSpecT = function (dictFunctor) {
    return Control_Monad_Writer_Trans.functorWriterT(dictFunctor);
};
var focus = function () {
    return function (dictMonad) {
        return over(SpecT)(Control_Monad_Writer_Trans.mapWriterT(Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(map(function (xs) {
            var $222 = any(any1((function () {
                var $229 = un(Test_Spec_Tree.Item);
                return function ($230) {
                    return (function (v) {
                        return v.isFocused;
                    })($229($230));
                };
            })()))(xs);
            if ($222) {
                return xs;
            };
            return map1(bimap(identity)(function (v) {
                return {
                    isFocused: true,
                    isParallelizable: v.isParallelizable,
                    example: v.example
                };
            }))(xs);
        }))));
    };
};
var focus1 = /* #__PURE__ */ focus();
var exampleMUnit = {
    evaluateExample: function (t) {
        return function (around$prime) {
            return around$prime(function (v) {
                return t;
            });
        };
    }
};
var exampleFunc = {
    evaluateExample: function (t) {
        return function (around$prime) {
            return around$prime(t);
        };
    }
};
var evaluateExample = function (dict) {
    return dict.evaluateExample;
};
var it = function (dictMonad) {
    var tell = Control_Monad_Writer_Class.tell(monadTellWriterT(dictMonad));
    return function (dictExample) {
        var evaluateExample1 = evaluateExample(dictExample);
        return function (name) {
            return function (test) {
                return tell([ new Test_Spec_Tree.Leaf(name, new Data_Maybe.Just({
                    isParallelizable: Data_Maybe.Nothing.value,
                    isFocused: false,
                    example: evaluateExample1(test)
                })) ]);
            };
        };
    };
};
var itOnly = function () {
    return function (dictMonad) {
        var focus2 = focus1(dictMonad);
        var it1 = it(dictMonad);
        return function (dictExample) {
            var $231 = map2(focus2);
            var $232 = it1(dictExample);
            return function ($233) {
                return $231($232($233));
            };
        };
    };
};
var describe = function (dictMonad) {
    var map3 = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
    return function (name) {
        return over(SpecT)(Control_Monad_Writer_Trans.mapWriterT(map3(map(function (group) {
            return [ new Test_Spec_Tree.Node(new Data_Either.Left(name), group) ];
        }))));
    };
};
var describeOnly = function () {
    return function (dictMonad) {
        var $234 = map2(focus1(dictMonad));
        var $235 = describe(dictMonad);
        return function ($236) {
            return $234($235($236));
        };
    };
};
var collect = function (dictFunctor) {
    var $237 = Data_Functor.map(dictFunctor)(Test_Spec_Tree.discardUnfocused);
    var $238 = Control_Monad_Writer_Trans.execWriterT(dictFunctor);
    var $239 = un(SpecT);
    return function ($240) {
        return $237($238($239($240)));
    };
};
var bindSpecT = function (dictBind) {
    return bindWriterT(dictBind);
};
var aroundWith = function (dictMonad) {
    var mapSpecTree1 = mapSpecTree(((dictMonad.Bind1()).Apply0()).Functor0());
    return function (action) {
        return mapSpecTree1(identity)(bimap(action)(Test_Spec_Tree.modifyAroundAction(action)));
    };
};
var around_ = function (dictMonad) {
    var aroundWith1 = aroundWith(dictMonad);
    return function (action) {
        return aroundWith1(function (e) {
            return function (a) {
                return action(e(a));
            };
        });
    };
};
var before_ = function (dictMonad) {
    var around_1 = around_(dictMonad);
    return function (dictMonad1) {
        var applySecond = Control_Apply.applySecond((dictMonad1.Bind1()).Apply0());
        return function (action) {
            return around_1(function (v) {
                return applySecond(action)(v);
            });
        };
    };
};
var beforeAll_ = function (dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var bind = Control_Bind.bind(bindSpecT(Monad0.Bind1()));
    var liftEffect = Effect_Class.liftEffect(monadEffectWriter(dictMonadEffect));
    var before_1 = before_(Monad0);
    return function (dictMonadAff) {
        var before_2 = before_1((dictMonadAff.MonadEffect0()).Monad0());
        var memoize1 = memoize(dictMonadAff);
        return function (dictMonadError) {
            var memoize2 = memoize1(dictMonadError);
            return function (action) {
                return function (spec) {
                    return bind(liftEffect(Effect_AVar["new"](MEmpty.value)))(function ($$var) {
                        return before_2(memoize2($$var)(action))(spec);
                    });
                };
            };
        };
    };
};
var beforeWith = function (dictMonad) {
    var aroundWith1 = aroundWith(dictMonad);
    return function (dictMonad1) {
        var bind = Control_Bind.bind(dictMonad1.Bind1());
        return function (action) {
            return aroundWith1(function (e) {
                return function (x) {
                    return bind(action(x))(e);
                };
            });
        };
    };
};
var around = function (dictMonad) {
    var aroundWith1 = aroundWith(dictMonad);
    return function (action) {
        return aroundWith1(function (e) {
            return function (v) {
                return action(e);
            };
        });
    };
};
var before = function (dictMonad) {
    var around1 = around(dictMonad);
    return function (dictMonad1) {
        var bind = Control_Bind.bind(dictMonad1.Bind1());
        return function (action) {
            return around1(function (v) {
                return bind(action)(v);
            });
        };
    };
};
var beforeAll = function (dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var bind = Control_Bind.bind(bindSpecT(Monad0.Bind1()));
    var liftEffect = Effect_Class.liftEffect(monadEffectWriter(dictMonadEffect));
    var before1 = before(Monad0);
    return function (dictMonadAff) {
        var before2 = before1((dictMonadAff.MonadEffect0()).Monad0());
        var memoize1 = memoize(dictMonadAff);
        return function (dictMonadError) {
            var memoize2 = memoize1(dictMonadError);
            return function (action) {
                return function (spec) {
                    return bind(liftEffect(Effect_AVar["new"](MEmpty.value)))(function ($$var) {
                        return before2(memoize2($$var)(action))(spec);
                    });
                };
            };
        };
    };
};
var applySpecT = function (dictApply) {
    return applyWriterT(dictApply);
};
var applicativeSpecT = function (dictApplicative) {
    return applicativeWriterT(dictApplicative);
};
var alternativeSpecT = function (dictAlternative) {
    return alternativeWriterT(dictAlternative);
};
var altSpecT = function (dictAlt) {
    return Control_Monad_Writer_Trans.altWriterT(dictAlt);
};
var afterAll = function (dictMonad) {
    var map3 = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
    return function (action) {
        return over(SpecT)(Control_Monad_Writer_Trans.mapWriterT(map3(map(function (group) {
            return [ new Test_Spec_Tree.Node(new Data_Either.Right(action), group) ];
        }))));
    };
};
var afterAll_ = function (dictMonad) {
    var afterAll1 = afterAll(dictMonad);
    return function (action) {
        return afterAll1(Data_Function["const"](action));
    };
};
var after = function (dictMonad) {
    var aroundWith1 = aroundWith(dictMonad);
    return function (dictMonadBracket) {
        var bracket = Control_Monad_Fork_Class.bracket(dictMonadBracket);
        var pure = Control_Applicative.pure((((dictMonadBracket.MonadError1()).MonadThrow0()).Monad0()).Applicative0());
        return function (action) {
            var $$finally = function (act) {
                return function (fin) {
                    return bracket(pure(Data_Unit.unit))(function (v) {
                        return function (v1) {
                            return fin;
                        };
                    })(Data_Function["const"](act));
                };
            };
            return aroundWith1(function (e) {
                return function (x) {
                    return $$finally(e(x))(action(x));
                };
            });
        };
    };
};
var after_ = function (dictMonad) {
    var after1 = after(dictMonad);
    return function (dictMonadBracket) {
        var after2 = after1(dictMonadBracket);
        return function (action) {
            return after2(function (v) {
                return action;
            });
        };
    };
};
export {
    SpecT,
    mapSpecTree,
    collect,
    CleanUpWithContext,
    TestWithName,
    hoistSpec,
    evaluateExample,
    parallel,
    sequential,
    focus,
    describeOnly,
    itOnly,
    describe,
    it,
    pending,
    pending$prime,
    aroundWith,
    around,
    around_,
    before,
    before_,
    beforeWith,
    beforeAll,
    beforeAll_,
    after,
    after_,
    afterAll,
    afterAll_,
    newtypeSpecT,
    functorSpecT,
    applySpecT,
    applicativeSpecT,
    altSpecT,
    plusSpecT,
    alternativeSpecT,
    bindSpecT,
    monadSpecT,
    monadRecSpecT,
    monadPlusSpecT,
    monadTransSpecT,
    monadEffectWriter,
    monadContSpecT,
    monadThrowSpecT,
    monadErrorSpecT,
    monadAskSpecT,
    monadReaderSpecT,
    monadStateSpecT,
    exampleFunc,
    exampleMUnit,
    warn
};
export {
    Item,
    Leaf,
    Node
} from "../Test.Spec.Tree/index.js";
//# sourceMappingURL=index.js.map
