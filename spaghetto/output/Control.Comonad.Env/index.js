// Generated by purs version 0.15.10
import * as Control_Comonad_Env_Class from "../Control.Comonad.Env.Class/index.js";
import * as Control_Comonad_Env_Trans from "../Control.Comonad.Env.Trans/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Tuple.functorTuple);
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var withEnv = Control_Comonad_Env_Trans.withEnvT;
var runEnv = function (v) {
    return map(unwrap)(v);
};
var mapEnv = /* #__PURE__ */ Data_Functor.map(/* #__PURE__ */ Control_Comonad_Env_Trans.functorEnvT(Data_Identity.functorIdentity));
var env = function (e) {
    return function (a) {
        return new Data_Tuple.Tuple(e, a);
    };
};
export {
    runEnv,
    withEnv,
    mapEnv,
    env
};
export {
    ask,
    asks,
    local
} from "../Control.Comonad.Env.Class/index.js";
export {
    EnvT,
    mapEnvT,
    runEnvT,
    withEnvT
} from "../Control.Comonad.Env.Trans/index.js";
//# sourceMappingURL=index.js.map
