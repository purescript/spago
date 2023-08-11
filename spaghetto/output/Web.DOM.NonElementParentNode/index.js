// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Nullable from "../Data.Nullable/index.js";
import * as Effect from "../Effect/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Effect.functorEffect);
var getElementById = function (eid) {
    var $2 = map(Data_Nullable.toMaybe);
    var $3 = $foreign["_getElementById"](eid);
    return function ($4) {
        return $2($3($4));
    };
};
export {
    getElementById
};
//# sourceMappingURL=index.js.map
