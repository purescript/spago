// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Control_Monad_Cont_Class from "../Control.Monad.Cont.Class/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Control_Monad_Writer_Class from "../Control.Monad.Writer.Class/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var MaybeT = function (x) {
    return x;
};
var runMaybeT = function (v) {
    return v;
};
var newtypeMaybeT = {
    Coercible0: function () {
        return undefined;
    }
};
var monadTransMaybeT = {
    lift: function (dictMonad) {
        var $157 = Control_Monad.liftM1(dictMonad)(Data_Maybe.Just.create);
        return function ($158) {
            return MaybeT($157($158));
        };
    }
};
var lift = /* #__PURE__ */ Control_Monad_Trans_Class.lift(monadTransMaybeT);
var mapMaybeT = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorMaybeT = function (dictFunctor) {
    var map1 = Data_Functor.map(dictFunctor);
    return {
        map: function (f) {
            return function (v) {
                return map1(map(f))(v);
            };
        }
    };
};
var monadMaybeT = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeMaybeT(dictMonad);
        },
        Bind1: function () {
            return bindMaybeT(dictMonad);
        }
    };
};
var bindMaybeT = function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return {
        bind: function (v) {
            return function (f) {
                return bind(v)(function (v1) {
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return pure(Data_Maybe.Nothing.value);
                    };
                    if (v1 instanceof Data_Maybe.Just) {
                        var v2 = f(v1.value0);
                        return v2;
                    };
                    throw new Error("Failed pattern match at Control.Monad.Maybe.Trans (line 54, column 11 - line 56, column 42): " + [ v1.constructor.name ]);
                });
            };
        },
        Apply0: function () {
            return applyMaybeT(dictMonad);
        }
    };
};
var applyMaybeT = function (dictMonad) {
    var functorMaybeT1 = functorMaybeT(((dictMonad.Bind1()).Apply0()).Functor0());
    return {
        apply: Control_Monad.ap(monadMaybeT(dictMonad)),
        Functor0: function () {
            return functorMaybeT1;
        }
    };
};
var applicativeMaybeT = function (dictMonad) {
    return {
        pure: (function () {
            var $159 = Control_Applicative.pure(dictMonad.Applicative0());
            return function ($160) {
                return MaybeT($159(Data_Maybe.Just.create($160)));
            };
        })(),
        Apply0: function () {
            return applyMaybeT(dictMonad);
        }
    };
};
var semigroupMaybeT = function (dictMonad) {
    var lift2 = Control_Apply.lift2(applyMaybeT(dictMonad));
    return function (dictSemigroup) {
        return {
            append: lift2(Data_Semigroup.append(dictSemigroup))
        };
    };
};
var monadAskMaybeT = function (dictMonadAsk) {
    var Monad0 = dictMonadAsk.Monad0();
    var monadMaybeT1 = monadMaybeT(Monad0);
    return {
        ask: lift(Monad0)(Control_Monad_Reader_Class.ask(dictMonadAsk)),
        Monad0: function () {
            return monadMaybeT1;
        }
    };
};
var monadReaderMaybeT = function (dictMonadReader) {
    var local = Control_Monad_Reader_Class.local(dictMonadReader);
    var monadAskMaybeT1 = monadAskMaybeT(dictMonadReader.MonadAsk0());
    return {
        local: function (f) {
            return mapMaybeT(local(f));
        },
        MonadAsk0: function () {
            return monadAskMaybeT1;
        }
    };
};
var monadContMaybeT = function (dictMonadCont) {
    var callCC = Control_Monad_Cont_Class.callCC(dictMonadCont);
    var monadMaybeT1 = monadMaybeT(dictMonadCont.Monad0());
    return {
        callCC: function (f) {
            return callCC(function (c) {
                var v = f(function (a) {
                    return c(new Data_Maybe.Just(a));
                });
                return v;
            });
        },
        Monad0: function () {
            return monadMaybeT1;
        }
    };
};
var monadEffectMaybe = function (dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var monadMaybeT1 = monadMaybeT(Monad0);
    return {
        liftEffect: (function () {
            var $161 = lift(Monad0);
            var $162 = Effect_Class.liftEffect(dictMonadEffect);
            return function ($163) {
                return $161($162($163));
            };
        })(),
        Monad0: function () {
            return monadMaybeT1;
        }
    };
};
var monadRecMaybeT = function (dictMonadRec) {
    var tailRecM = Control_Monad_Rec_Class.tailRecM(dictMonadRec);
    var Monad0 = dictMonadRec.Monad0();
    var bind = Control_Bind.bind(Monad0.Bind1());
    var pure = Control_Applicative.pure(Monad0.Applicative0());
    var monadMaybeT1 = monadMaybeT(Monad0);
    return {
        tailRecM: function (f) {
            var $164 = tailRecM(function (a) {
                var v = f(a);
                return bind(v)(function (m$prime) {
                    return pure((function () {
                        if (m$prime instanceof Data_Maybe.Nothing) {
                            return new Control_Monad_Rec_Class.Done(Data_Maybe.Nothing.value);
                        };
                        if (m$prime instanceof Data_Maybe.Just && m$prime.value0 instanceof Control_Monad_Rec_Class.Loop) {
                            return new Control_Monad_Rec_Class.Loop(m$prime.value0.value0);
                        };
                        if (m$prime instanceof Data_Maybe.Just && m$prime.value0 instanceof Control_Monad_Rec_Class.Done) {
                            return new Control_Monad_Rec_Class.Done(new Data_Maybe.Just(m$prime.value0.value0));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Maybe.Trans (line 82, column 16 - line 85, column 43): " + [ m$prime.constructor.name ]);
                    })());
                });
            });
            return function ($165) {
                return MaybeT($164($165));
            };
        },
        Monad0: function () {
            return monadMaybeT1;
        }
    };
};
var monadStateMaybeT = function (dictMonadState) {
    var Monad0 = dictMonadState.Monad0();
    var lift1 = lift(Monad0);
    var state = Control_Monad_State_Class.state(dictMonadState);
    var monadMaybeT1 = monadMaybeT(Monad0);
    return {
        state: function (f) {
            return lift1(state(f));
        },
        Monad0: function () {
            return monadMaybeT1;
        }
    };
};
var monadTellMaybeT = function (dictMonadTell) {
    var Monad1 = dictMonadTell.Monad1();
    var Semigroup0 = dictMonadTell.Semigroup0();
    var monadMaybeT1 = monadMaybeT(Monad1);
    return {
        tell: (function () {
            var $166 = lift(Monad1);
            var $167 = Control_Monad_Writer_Class.tell(dictMonadTell);
            return function ($168) {
                return $166($167($168));
            };
        })(),
        Semigroup0: function () {
            return Semigroup0;
        },
        Monad1: function () {
            return monadMaybeT1;
        }
    };
};
var monadWriterMaybeT = function (dictMonadWriter) {
    var MonadTell1 = dictMonadWriter.MonadTell1();
    var Monad1 = MonadTell1.Monad1();
    var bind = Control_Bind.bind(Monad1.Bind1());
    var listen = Control_Monad_Writer_Class.listen(dictMonadWriter);
    var pure = Control_Applicative.pure(Monad1.Applicative0());
    var pass = Control_Monad_Writer_Class.pass(dictMonadWriter);
    var Monoid0 = dictMonadWriter.Monoid0();
    var monadTellMaybeT1 = monadTellMaybeT(MonadTell1);
    return {
        listen: mapMaybeT(function (m) {
            return bind(listen(m))(function (v) {
                return pure(map(function (r) {
                    return new Data_Tuple.Tuple(r, v.value1);
                })(v.value0));
            });
        }),
        pass: mapMaybeT(function (m) {
            return pass(bind(m)(function (a) {
                return pure((function () {
                    if (a instanceof Data_Maybe.Nothing) {
                        return new Data_Tuple.Tuple(Data_Maybe.Nothing.value, identity);
                    };
                    if (a instanceof Data_Maybe.Just) {
                        return new Data_Tuple.Tuple(new Data_Maybe.Just(a.value0.value0), a.value0.value1);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Maybe.Trans (line 119, column 10 - line 121, column 43): " + [ a.constructor.name ]);
                })());
            }));
        }),
        Monoid0: function () {
            return Monoid0;
        },
        MonadTell1: function () {
            return monadTellMaybeT1;
        }
    };
};
var monadThrowMaybeT = function (dictMonadThrow) {
    var Monad0 = dictMonadThrow.Monad0();
    var lift1 = lift(Monad0);
    var throwError = Control_Monad_Error_Class.throwError(dictMonadThrow);
    var monadMaybeT1 = monadMaybeT(Monad0);
    return {
        throwError: function (e) {
            return lift1(throwError(e));
        },
        Monad0: function () {
            return monadMaybeT1;
        }
    };
};
var monadErrorMaybeT = function (dictMonadError) {
    var catchError = Control_Monad_Error_Class.catchError(dictMonadError);
    var monadThrowMaybeT1 = monadThrowMaybeT(dictMonadError.MonadThrow0());
    return {
        catchError: function (v) {
            return function (h) {
                return catchError(v)(function (a) {
                    var v1 = h(a);
                    return v1;
                });
            };
        },
        MonadThrow0: function () {
            return monadThrowMaybeT1;
        }
    };
};
var monoidMaybeT = function (dictMonad) {
    var pure = Control_Applicative.pure(applicativeMaybeT(dictMonad));
    var semigroupMaybeT1 = semigroupMaybeT(dictMonad);
    return function (dictMonoid) {
        var semigroupMaybeT2 = semigroupMaybeT1(dictMonoid.Semigroup0());
        return {
            mempty: pure(Data_Monoid.mempty(dictMonoid)),
            Semigroup0: function () {
                return semigroupMaybeT2;
            }
        };
    };
};
var altMaybeT = function (dictMonad) {
    var Bind1 = dictMonad.Bind1();
    var bind = Control_Bind.bind(Bind1);
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    var functorMaybeT1 = functorMaybeT((Bind1.Apply0()).Functor0());
    return {
        alt: function (v) {
            return function (v1) {
                return bind(v)(function (m) {
                    if (m instanceof Data_Maybe.Nothing) {
                        return v1;
                    };
                    return pure(m);
                });
            };
        },
        Functor0: function () {
            return functorMaybeT1;
        }
    };
};
var plusMaybeT = function (dictMonad) {
    var altMaybeT1 = altMaybeT(dictMonad);
    return {
        empty: Control_Applicative.pure(dictMonad.Applicative0())(Data_Maybe.Nothing.value),
        Alt0: function () {
            return altMaybeT1;
        }
    };
};
var alternativeMaybeT = function (dictMonad) {
    var applicativeMaybeT1 = applicativeMaybeT(dictMonad);
    var plusMaybeT1 = plusMaybeT(dictMonad);
    return {
        Applicative0: function () {
            return applicativeMaybeT1;
        },
        Plus1: function () {
            return plusMaybeT1;
        }
    };
};
var monadPlusMaybeT = function (dictMonad) {
    var monadMaybeT1 = monadMaybeT(dictMonad);
    var alternativeMaybeT1 = alternativeMaybeT(dictMonad);
    return {
        Monad0: function () {
            return monadMaybeT1;
        },
        Alternative1: function () {
            return alternativeMaybeT1;
        }
    };
};
export {
    MaybeT,
    runMaybeT,
    mapMaybeT,
    newtypeMaybeT,
    functorMaybeT,
    applyMaybeT,
    applicativeMaybeT,
    bindMaybeT,
    monadMaybeT,
    monadTransMaybeT,
    altMaybeT,
    plusMaybeT,
    alternativeMaybeT,
    monadPlusMaybeT,
    monadRecMaybeT,
    monadEffectMaybe,
    monadContMaybeT,
    monadThrowMaybeT,
    monadErrorMaybeT,
    monadAskMaybeT,
    monadReaderMaybeT,
    monadStateMaybeT,
    monadTellMaybeT,
    monadWriterMaybeT,
    semigroupMaybeT,
    monoidMaybeT
};
export {
    lift
} from "../Control.Monad.Trans.Class/index.js";
//# sourceMappingURL=index.js.map
