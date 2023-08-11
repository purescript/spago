// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_DateTime_Instant from "../Data.DateTime.Instant/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Effect from "../Effect/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Effect.functorEffect);
var nowTime = /* #__PURE__ */ map(function ($2) {
    return Data_DateTime.time(Data_DateTime_Instant.toDateTime($2));
})($foreign.now);
var nowDateTime = /* #__PURE__ */ map(Data_DateTime_Instant.toDateTime)($foreign.now);
var nowDate = /* #__PURE__ */ map(function ($3) {
    return Data_DateTime.date(Data_DateTime_Instant.toDateTime($3));
})($foreign.now);
export {
    now,
    getTimezoneOffset
} from "./foreign.js";
export {
    nowDateTime,
    nowDate,
    nowTime
};
//# sourceMappingURL=index.js.map
